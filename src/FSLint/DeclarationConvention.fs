module B2R2.FSLint.DeclarationConvention

open System
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

/// Adjusts actual spacing by subtracting comment lines
/// TODO: Condition directives checks
let private adjustByComment trivia prev next expectedSpacing actualSpacing =
  if Option.isSome (findDirectivesBetween trivia prev next) then expectedSpacing
  else actualSpacing - countCommentLines trivia prev next

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

let checkEqualSpacing src trivia patRange equalRange bodyRange retInfo =
  let patRange =
    if Option.isSome (retInfo: option<SynBindingReturnInfo>) then
      let SynBindingReturnInfo(range = range) = retInfo.Value
      range
    else
      patRange
  if (patRange: range).EndLine = (bodyRange: range).StartLine then
    if patRange.EndColumn + 1 <> (equalRange: range).StartColumn then
      let commentBeforeEqual =
        findCommentsBetween trivia patRange.EndRange equalRange.StartRange
      if Option.isNone commentBeforeEqual then
        Range.mkRange "" patRange.End equalRange.Start
        |> reportEqaulBeforeSpacing src
      else
        ()
    else
      ()
    if equalRange.EndColumn + 1 <> bodyRange.StartColumn then
      let commentAfterEqual =
        findCommentsBetween trivia equalRange.EndRange bodyRange.StartRange
      if Option.isNone commentAfterEqual then
        Range.mkRange "" equalRange.End bodyRange.Start
        |> reportEqaulAfterSpacing src
      else
        ()
    else
      ()
    if patRange.EndColumn = equalRange.StartColumn
      && equalRange.EndColumn = bodyRange.StartColumn then
      Range.mkRange "" patRange.End bodyRange.Start
      |> fun range -> reportWarn src range "Use single whitespace around '='"
    else
      ()
  else
    if patRange.EndLine = equalRange.StartLine then
      if patRange.EndColumn + 1 <> equalRange.StartColumn then
        let commentBeforeEqual =
          findCommentsBetween trivia patRange.EndRange equalRange.StartRange
        if Option.isNone commentBeforeEqual then
          Range.mkRange "" patRange.End equalRange.Start
          |> reportEqaulBeforeSpacing src
        else
          ()
      else
        ()
    else
      if equalRange.EndColumn + 1 <> bodyRange.StartColumn then
        let commentAfterEqual =
          findCommentsBetween trivia equalRange.EndRange bodyRange.StartRange
        if Option.isNone commentAfterEqual then
          Range.mkRange "" equalRange.End bodyRange.Start
          |> reportEqaulAfterSpacing src
        else
          ()
      else
        ()

let checkLetAndMultilineRhsPlacement (src: ISourceText) (binding: SynBinding) =
  let SynBinding(expr = body; trivia = trivia) = binding
  match trivia.EqualsRange with
  | Some eqRange ->
    match body with
    | SynExpr.Const(SynConst.String(synStringKind = stringKind), _)
      when stringKind = SynStringKind.TripleQuote
      && eqRange.StartLine = body.Range.StartLine
      && (body.Range.StartLine <> body.Range.EndLine) ->
        reportWarn src body.Range "Move '\"\"\"' to next line"
    | _ ->
      ()
  | None ->
    ()

let checkAttributesLineSpacing src trivia attrs (moduleRange: range) =
  let lastAttr = List.tryLast (attrs: SynAttributes)
  if Option.isSome lastAttr then
    let attrRange = lastAttr.Value.Range
    let hasComments = findCommentsBetween trivia attrRange moduleRange
    if Option.isNone hasComments
      && attrRange.EndLine + 1 <> moduleRange.StartLine
      && attrRange.StartLine <> moduleRange.StartLine then
      Range.mkRange "" (Position.mkPos (moduleRange.StartLine - 1) 0)
        moduleRange.Start
      |> reportNewLine src
    else
      ()
  else
    ()

let checkComputationExprPlacement (src: ISourceText) (binding: SynBinding) =
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

let check (src: ISourceText) decls codeTrivia =
  decls
  |> List.pairwise
  |> List.iter (fun (prevDecl: SynModuleDecl, nextDecl) ->
    if prevDecl.IsLet && nextDecl.IsLet then
      let expected = calculateSpacingBetweenDecls src prevDecl nextDecl
      let actual =
        nextDecl.Range.StartLine - prevDecl.Range.EndLine
        |> adjustByComment codeTrivia prevDecl.Range nextDecl.Range expected
      if actual <> expected then
        if expected - actual = 1 then
          Range.unionRanges prevDecl.Range nextDecl.Range
          |> fun range -> reportWarn src range "Use single blank line"
        else
          Range.mkRange "" (Position.mkPos (prevDecl.Range.EndLine + 1) 0)
            (Position.mkPos (nextDecl.Range.StartLine - 1) 0)
          |> fun range -> reportWarn src range "Use single blank line"
      else
        ()
    else
      ()
  )