module B2R2.FSLint.DeclarationConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let private tryFindBlockCommentEndAfter lines startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip (startIdx + 1)
  |> Array.tryFind (fun (_, line: string) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "*)" || line.TrimEnd().EndsWith "*)")
  |> Option.map fst

let private countLeadingDocComments lines startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip startIdx
  |> Array.takeWhile (fun (_, line: string) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "///" || trimmed = "")
  |> Array.filter (fun (_, line) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "///")
  |> Array.length

let private countLeadingRegularComments lines startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip startIdx
  |> Array.takeWhile (fun (_, line: string) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "//" && not (trimmed.StartsWith "///")
    || trimmed = "")
  |> Array.filter (fun (_, line) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "//" && not (trimmed.StartsWith "///"))
  |> Array.length

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

/// Determines line breaks between declarations based on their types.
/// Skips validation if compiler directives are present.
/// Adjusts range when handling block comments (* *) or doc comments (///).
let private adjustByComment src prevRange nextRange expect actual =
  let createRangeBetweenDecl =
    Range.mkRange "" (Position.mkPos (prevRange: range).EndLine 0)
      (Position.mkPos (nextRange: range).StartLine 0)
  let subStr = (src: ISourceText).GetSubTextFromRange createRangeBetweenDecl
  if subStr.Contains("#") then
    expect
  else
    let lines =
      subStr.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
      |> Array.map (fun line -> line.Trim('\r'))
    if subStr.Trim() = "" || lines.Length <= 1 then
      actual
    else
      let blockCommentStart =
        lines
        |> Array.tryFindIndex (fun line ->
          (line.TrimStart()).StartsWith "(*")
      let docCommentStart =
        lines
        |> Array.tryFindIndex (fun line ->
          (line.TrimStart()).StartsWith "///")
      let regularCommentStart =
        lines
        |> Array.tryFindIndex (fun line ->
          let trimmed = line.TrimStart()
          trimmed.StartsWith "//" && not (trimmed.StartsWith "///"))
      match blockCommentStart, docCommentStart, regularCommentStart with
      | Some startIdx, _, _ ->
        match tryFindBlockCommentEndAfter lines startIdx with
        | Some endIdx ->
          actual - (endIdx - startIdx + 1)
        | None ->
          actual - (lines.Length - startIdx - 1)
      | _, Some startIdx, _ ->
        actual - countLeadingDocComments lines startIdx
      | _, _, Some startIdx ->
        actual - countLeadingRegularComments lines startIdx
      | _ -> actual

let checkEqualSpacing src patRange (equalRange: range) bodyRange retInfo =
  let patRange =
    if Option.isSome (retInfo: option<SynBindingReturnInfo>) then
      let SynBindingReturnInfo(range = range) = retInfo.Value
      range
    else
      patRange
  if (patRange: range).EndLine = (bodyRange: range).StartLine then
    if patRange.EndColumn + 1 <> equalRange.StartColumn then
      let gap = Range.unionRanges patRange.EndRange equalRange.StartRange
      let gapStr = gap |> (src: ISourceText).GetSubTextFromRange
      if gapStr.Contains "(*" then ()
      else
        Range.mkRange "" patRange.End equalRange.Start
        |> fun range ->
          reportWarn src range "Use single whitespace before '='"
    elif equalRange.EndColumn + 1 <> bodyRange.StartColumn then
      Range.mkRange "" equalRange.End bodyRange.Start
      |> fun range -> reportWarn src range "Use single whitespace after '='"
    elif patRange.EndColumn = equalRange.StartColumn
      && equalRange.EndColumn = bodyRange.StartColumn then
      Range.mkRange "" patRange.End bodyRange.Start
      |> fun range -> reportWarn src range "Use single whitespace around '='"
    else
      ()
  else
    if patRange.EndLine = equalRange.StartLine then
      if patRange.EndColumn + 1 <> equalRange.StartColumn then
        let gap = Range.unionRanges patRange.EndRange equalRange.StartRange
        let gapStr = gap |> src.GetSubTextFromRange
        if gapStr.Contains "(*" then ()
        else
          Range.mkRange "" patRange.End equalRange.Start
          |> fun range ->
            reportWarn src range "Use single whitespace before '='"
    else
      if equalRange.EndColumn + 1 <> bodyRange.StartColumn then
        Range.mkRange "" equalRange.End bodyRange.Start
        |> fun range -> reportWarn src range "Use single whitespace after '='"

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

let checkUnnecessaryLineBreak (src: ISourceText) (binding: SynBinding) =
  let SynBinding(headPat = pattern; expr = body; trivia = trivia) = binding
  let compileFlag =
    Range.unionRanges pattern.Range.StartRange body.Range.StartRange
    |> src.GetSubTextFromRange
    |> fun str -> str.ToCharArray() |> Array.contains '#'
  if body.Range.StartLine = body.Range.EndLine && compileFlag |> not then
    match trivia.EqualsRange with
    | Some eqRange ->
      if eqRange.EndLine < body.Range.StartLine then
        let fullRange =
          Range.mkRange ""
            (Position.mkPos pattern.Range.StartLine 0)
            (Position.mkPos body.Range.EndLine
              (src.GetLineString(body.Range.EndLine - 1).Length))
        let declText = src.GetSubTextFromRange fullRange
        let lines =
          declText.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
        let indent = pattern.Range.StartColumn
        let contentParts =
          lines
          |> Array.map (fun line -> line.Trim())
          |> Array.filter (fun line -> line <> "")
        let oneLine = String.concat " " contentParts
        let totalLength = indent + oneLine.Length
        if totalLength <= MaxLineLength then
          reportWarn src body.Range "Remove unnecessary line break"
      else
        ()
    | None ->
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

let check (src: ISourceText) decls =
  decls
  |> List.pairwise
  |> List.iter (fun (prevDecl: SynModuleDecl, nextDecl) ->
    if prevDecl.IsLet && nextDecl.IsLet then
      let expectedSpacing = calculateSpacingBetweenDecls src prevDecl nextDecl
      let actualSpacing =
        nextDecl.Range.StartLine - prevDecl.Range.EndLine
        |> adjustByComment src prevDecl.Range nextDecl.Range expectedSpacing
      if actualSpacing <> expectedSpacing then
        reportWarn src nextDecl.Range "Use single blank line"
      else
        ()
    else
      ()
  )