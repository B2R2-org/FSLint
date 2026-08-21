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