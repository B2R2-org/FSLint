module B2R2.FSLint.DeclarationConvention

open System
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

/// Adjusts actual spacing by subtracting comment lines
/// TODO: Condition directives checks
let private adjustByComment prev next expectedSpacing actualSpacing =
  if Option.isSome (findDirectivesBetween prev next) then expectedSpacing
  else actualSpacing - countCommentLines prev next

let private calculateSpacingBetweenDecls (src: ISourceText) prevDecl nextDecl =
  let normalCase =
    match prevDecl, nextDecl with
    | SynModuleDecl.Attributes _, _
    | SynModuleDecl.Open _, SynModuleDecl.Open _ -> 1
    | _ -> 2
  let lastLineStr = src.GetLineString(prevDecl.Range.EndLine - 1)
  if lastLineStr.TrimStart().StartsWith "[<"
    && lastLineStr.TrimStart().EndsWith ">]"
    && normalCase = 2
  then normalCase - 1
  else normalCase

/// The gap before `=`. A comment standing in it is a layout of its own and
/// what it does to the spacing is not the author's doing.
let private checkBeforeEqual src (patRange: range) (equalRange: range) =
  if patRange.EndColumn + 1 = equalRange.StartColumn then
    ()
  else
    match findCommentsBetween patRange.EndRange equalRange.StartRange with
    | Some _ ->
      ()
    | None ->
      Range.mkRange "" patRange.End equalRange.Start
      |> reportEqaulBeforeSpacing src

/// The gap after `=`, read the same way.
let private checkAfterEqual src (equalRange: range) (bodyRange: range) =
  if equalRange.EndColumn + 1 = bodyRange.StartColumn then
    ()
  else
    match findCommentsBetween equalRange.EndRange bodyRange.StartRange with
    | Some _ ->
      ()
    | None ->
      Range.mkRange "" equalRange.End bodyRange.Start
      |> reportEqaulAfterSpacing src

/// A binding written on one line answers for both sides of its `=`, and for
/// having no space at all on either. One broken across lines answers only for
/// the side the break did not fall on: where the body went down, the gap after
/// `=` is the break itself, and where the header went down, the gap before it
/// is.
let checkEqualSpacing src patRange (equalRange: range) bodyRange retInfo =
  let patRange =
    match (retInfo: option<SynBindingReturnInfo>) with
    | Some(SynBindingReturnInfo(range = range)) -> range
    | None -> patRange
  if (patRange: range).EndLine = (bodyRange: range).StartLine then
    checkBeforeEqual src patRange equalRange
    checkAfterEqual src equalRange bodyRange
    if patRange.EndColumn = equalRange.StartColumn
      && equalRange.EndColumn = bodyRange.StartColumn then
      Range.mkRange "" patRange.End bodyRange.Start
      |> fun range -> reportWarn src range "Use single whitespace around '='"
    else
      ()
  elif patRange.EndLine = equalRange.StartLine then
    checkBeforeEqual src patRange equalRange
  else
    checkAfterEqual src equalRange bodyRange

let [<Literal>] private BodyIndent = 2

/// Where the declaration holding a keyword begins, which is the indentation of
/// the row the keyword opens.
///
/// Not the keyword's own column: an access modifier can stand in front of it
/// (`private new(...)`), and so can an attribute written on the same row. The
/// body below is laid out against where the declaration begins, which is where
/// its row does.
let private declarationColumn (src: ISourceText) (keyword: range) =
  let text = src.GetLineString(keyword.StartLine - 1)
  min (text.Length - text.TrimStart().Length) keyword.StartColumn

/// True when a body left up beside its `=` is closed by a bracket standing on
/// a line of its own.
///
/// That bracket is what says the rows above it are a layout of their own rather
/// than the tail of something that ran past its line. It is also what keeps the
/// text of a multiline string and the arguments of a broken call out of this: a
/// string closes on its quotes and a call on its parenthesis, and neither is
/// read here.
let private isFenced (src: ISourceText) (body: range) =
  if body.EndLine <= body.StartLine + 1 then
    false
  else
    let last = src.GetLineString(body.EndLine - 1)
    let upTo = min body.EndColumn last.Length
    match last.Substring(0, upTo).Trim() with
    | "}"
    | "]"
    | "|]"
    | "|}" -> true
    | _ -> false

/// The first row from `fromLine` to `lastLine` that holds code, and how far in
/// it opens. Blank rows are passed over, and so are rows a comment holds all
/// of.
///
/// A comment is prose. It is not laid out the way the code under it is, and
/// where it sits is the author's business -- so the comment standing above the
/// first case of a union or the first field of a record is not what the
/// indentation is read from. Reading it would leave the rule ambiguous: the
/// same code would pass or fail on where somebody put its comment.
let private firstCodeRow (src: ISourceText) fromLine lastLine =
  let mutable line = max 1 fromLine
  let mutable inBlock = false
  let mutable found = None
  while found.IsNone && line <= lastLine do
    let text = src.GetLineString(line - 1)
    let trimmed = text.TrimStart()
    let beyondComment (from: int) =
      match trimmed.IndexOf("*)", from) with
      | -1 -> ""
      | closed -> trimmed.Substring(closed + 2).Trim()
    let code =
      if inBlock then beyondComment 0
      elif trimmed.StartsWith "//" then ""
      elif trimmed.StartsWith "(*" then beyondComment 2
      else trimmed
    let opensBlock = inBlock || trimmed.StartsWith "(*"
    inBlock <- opensBlock && not (trimmed.Contains "*)")
    if code.Length > 0 then found <- Some(line, text.Length - trimmed.Length)
    else line <- line + 1
  found

/// A body whose bracket stayed up beside the `=` fences its rows under it, and
/// those rows are the body: the first of them opens two columns in from the
/// keyword, as a body sent below the `=` does. Only the first is read. A row
/// after it lines up with whatever the first one opened, and there is no
/// column this rule could ask of it.
let private checkFencedBody src (keyword: range) (body: range) =
  if isFenced src body |> not then
    ()
  else
    match firstCodeRow src (body.StartLine + 1) (body.EndLine - 1) with
    | None ->
      ()
    | Some(line, indent) ->
      if straddlesDirective body.StartLine line then
        ()
      elif indent = declarationColumn src keyword + BodyIndent then
        ()
      else
        (Position.mkPos line 0, Position.mkPos line indent)
        ||> Range.mkRange ""
        |> reportBodyIndent src

/// What a body has to answer for where it opens, given the keyword above it and
/// the `=` between them. An expression and the shape of a type are read the
/// same way.
///
/// The header is only asked once it stands on one line: a wrapped one puts the
/// rows under it in line with something inside itself rather than with the
/// keyword. From there the body went one of two ways -- down below the `=`, or
/// up beside it behind a bracket -- and each is read where it opens.
let private checkIndentOf src (keyword: range) (equals: range) (body: range) =
  if keyword.StartLine <> equals.EndLine then
    ()
  elif body.StartLine <= equals.EndLine then
    checkFencedBody src keyword body
  elif straddlesDirective equals.EndLine body.StartLine then
    ()
  else
    match firstCodeRow src body.StartLine body.EndLine with
    | None ->
      ()
    | Some(line, indent) ->
      if indent = declarationColumn src keyword + BodyIndent then
        ()
      else
        (Position.mkPos line 0, Position.mkPos line indent)
        ||> Range.mkRange ""
        |> reportBodyIndent src

/// The keywords whose body this rule reads: every one that opens a body the
/// author wrote and put an `=` in front of, which a member does as much as a
/// `let`. Two are left out, and neither for taste -- a synthetic keyword
/// belongs to no text, so there is no column to read a body against, and the
/// body of an `extern` is not in the file at all.
let private opensABody = function
  | SynLeadingKeyword.Synthetic
  | SynLeadingKeyword.Extern _ -> false
  | _ -> true

/// A body sent below its `=` opens two columns in from the keyword above it.
///
/// Only a binding whose header stands on one line is asked this. Once the
/// header wraps -- a parameter list too wide for its line, or an argument list
/// broken so that its parts agree -- the lines under it continue the header
/// and line up with something inside it rather than with the keyword. Where
/// they line up is a question for whatever broke them, and this rule has
/// nothing to say about it.
///
/// A body left up beside the `=` is not asked either: it has chosen no
/// indentation to answer for.
///
/// Nor is one reached across a conditional directive. What the parser reads as
/// the body then is whichever branch this build kept, and that branch opens
/// where the shape around it put it rather than where the binding did; the
/// body as written stands somewhere the parser never saw.
let checkBodyIndent src (trivia: SynBindingTrivia) (body: SynExpr) =
  if not isStrict then
    ()
  elif opensABody trivia.LeadingKeyword |> not then
    ()
  else
    match trivia.EqualsRange with
    | None ->
      ()
    | Some equals ->
      checkIndentOf src trivia.LeadingKeyword.Range equals body.Range

/// The keywords whose type body this rule reads. A synthetic one belongs to no
/// text, so there is no column to read it against.
let private opensAType = function
  | SynTypeDefnLeadingKeyword.Type _
  | SynTypeDefnLeadingKeyword.And _ -> true
  | _ -> false

/// A type whose shape is written below its `=` opens two columns in from the
/// keyword, as a binding body does. The cases of a union, the braces of a
/// record, the members of a class: what stands there is the body of the type,
/// and where it opens is read against the `type` above it.
let checkReprIndent src (trivia: SynTypeDefnTrivia) (repr: SynTypeDefnRepr) =
  if not isStrict then
    ()
  elif opensAType trivia.LeadingKeyword |> not then
    ()
  else
    match trivia.EqualsRange with
    | None ->
      ()
    | Some equals ->
      checkIndentOf src trivia.LeadingKeyword.Range equals repr.Range

let checkAttributesLineSpacing src attrs (moduleRange: range) =
  let lastAttr = List.tryLast (attrs: SynAttributes)
  if Option.isSome lastAttr then
    let attrRange = lastAttr.Value.Range
    if Option.isNone (findCommentsBetween attrRange moduleRange)
      && attrRange.EndLine + 1 <> moduleRange.StartLine
      && attrRange.StartLine <> moduleRange.StartLine then
      Range.mkRange ""
        (Position.mkPos (moduleRange.StartLine - 1) 0)
        moduleRange.Start
      |> reportNewLine src
    else
      ()
  else
    ()

let checkComputationExprPlacement (src: ISourceText) (binding: SynBinding) =
  if isStrict then
    let SynBinding(expr = body; trivia = trivia) = binding
    if trivia.EqualsRange.IsSome then
      match body with
      | SynExpr.ComputationExpr _
      | SynExpr.App(argExpr = SynExpr.ComputationExpr _)
        when trivia.EqualsRange.Value.EndLine = body.Range.StartLine ->
        reportWarn src body.Range "Move computation expression to next line"
      | _ ->
        ()
    else
      ()
  else
    ()

let checkSingleBlankLine (src: ISourceText) decls =
  if isStrict then
    decls
    |> List.pairwise
    |> List.iter (fun (prevDecl: SynModuleDecl, nextDecl) ->
      if prevDecl.IsLet && nextDecl.IsLet then
        let expected = calculateSpacingBetweenDecls src prevDecl nextDecl
        let actual =
          nextDecl.Range.StartLine - prevDecl.Range.EndLine
          |> adjustByComment prevDecl.Range nextDecl.Range expected
        if expected < actual then
          (Position.mkPos (prevDecl.Range.EndLine + 1) 0,
          Position.mkPos (nextDecl.Range.StartLine - 1) 0)
          ||> Range.mkRange prevDecl.Range.FileName
          |> fun range -> reportWarn src range "Use at most one blank line"
        else
          ()
      else
        ()
    )
  else
    ()