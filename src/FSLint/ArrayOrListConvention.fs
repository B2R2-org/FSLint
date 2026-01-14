module B2R2.FSLint.ArrayOrListConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let rec private collectElemAndOptionalSeparatorRanges acc = function
  | SynExpr.Sequential(expr1 = expr1; expr2 = expr2; trivia = trivia) ->
    trivia.SeparatorRange
    |> Option.map (fun sep -> sep :: expr1.Range :: acc)
    |> Option.defaultValue (expr1.Range :: acc)
    |> fun acc -> collectElemAndOptionalSeparatorRanges acc expr2
  | expr -> expr.Range :: acc |> List.rev

/// Checks proper spacing after semicolons between list/array elements.
/// Ensures exactly one space after each semicolon (e.g., "1; 2; 3").
let checkElementSpacing src (elemAndSepRanges: Range list) =
  for i in 0 .. 2 .. elemAndSepRanges.Length - 3 do
    let elemRange = elemAndSepRanges[i]
    let separatorRange = elemAndSepRanges[i + 1]
    let nextElement = elemAndSepRanges[i + 2]
    if elemRange.EndColumn <> separatorRange.StartColumn
    then
      Range.mkRange "" separatorRange.Start elemRange.End
      |> reportSemiColonBeforeSpacing src
    elif separatorRange.EndColumn = nextElement.StartColumn
      || separatorRange.EndColumn + 1 <> nextElement.StartColumn
    then
      Range.mkRange "" separatorRange.End nextElement.Start
      |> reportSemiColonAfterSpacing src
    else ()

let checkBracketComp (src: ISourceText) (elemRange: range) fullrange warnRange =
  let added =
    if src.GetLineString(elemRange.EndLine).TrimStart().StartsWith "(*" then 2
    else 1
  (Position.mkPos (elemRange.EndLine + added) 0,
    Position.mkPos (fullrange: range).EndLine 0)
  ||> Range.mkRange ""
  |> src.GetSubTextFromRange
  |> fun subStr ->
    subStr.Split([| '\n' |], StringSplitOptions.None)
    |> fun strArr ->
      let flagStartIsWrong =
        (Array.head strArr).TrimStart().StartsWith "#if" |> not
      let flagEndIsWrong = Array.last strArr |> String.IsNullOrEmpty |> not
      if flagEndIsWrong || flagStartIsWrong then
        reportBracketSpacingError src warnRange
      else
        ()

/// Checks proper spacing inside brackets for list/array literals.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing src distFstElemToOpenBracket (elemRange: range) range =
  if (range: range).StartColumn + distFstElemToOpenBracket
    <> elemRange.StartColumn then
    Range.mkRange "" range.Start elemRange.Start
    |> fun warnRange ->
      try checkBracketComp src elemRange range warnRange
      with _ -> reportBracketSpacingError src warnRange
  elif (range: range).EndColumn - distFstElemToOpenBracket
    <> elemRange.EndColumn then
    Range.mkRange "" elemRange.End range.End
    |> fun warnRange ->
    try checkBracketComp src elemRange range warnRange
    with _ -> reportBracketSpacingError src warnRange
  else
    ()

/// Checks proper spacing around range operator (..) in list/array literals.
/// Ensures single space before and after '..' (e.g., "1 .. 10" not "1..10").
let checkRangeOpSpacing src fstElem (rangeOfSecondElem: range) (opm: range) =
  match fstElem with
  | SynExpr.Const(range = rangeOfFirstElem) ->
    if opm.StartColumn = rangeOfFirstElem.EndColumn then
      Range.mkRange "" rangeOfFirstElem.End opm.Start
      |> reportRangeOperatorError src
    elif opm.EndColumn = rangeOfSecondElem.StartColumn then
      Range.mkRange "" opm.End rangeOfSecondElem.Start
      |> reportRangeOperatorError src
    else ()
  | SynExpr.Ident ident ->
    if opm.StartColumn = ident.idRange.EndColumn then
      Range.mkRange "" ident.idRange.End opm.Start
      |> reportRangeOperatorError src
    elif opm.EndColumn = rangeOfSecondElem.StartColumn then
      Range.mkRange "" opm.End rangeOfSecondElem.Start
      |> reportRangeOperatorError src
    else ()
  | SynExpr.IndexRange(opm = stepOpm
                       range1 = rangeOfFirstElem
                       range2 = stepRange) ->
    if stepOpm.StartColumn = rangeOfFirstElem.EndColumn then
      Range.mkRange "" rangeOfFirstElem.End stepOpm.Start
      |> reportRangeOperatorError src
    elif stepOpm.EndColumn = stepRange.StartColumn then
      Range.mkRange "" stepOpm.End stepRange.Start
      |> reportRangeOperatorError src
    elif opm.StartColumn = stepRange.EndColumn then
      Range.mkRange "" stepRange.End opm.Start
      |> reportRangeOperatorError src
    elif opm.EndColumn = rangeOfSecondElem.StartColumn then
      Range.mkRange "" opm.End rangeOfSecondElem.Start
      |> reportRangeOperatorError src
    else ()
  | _ -> ()

/// Checks proper spacing in empty list/array literals.
/// Ensures no space inside empty brackets (e.g., "[]" or "[||]" not "[ ]").
let checkEmpty src enclosureWidth (expr: SynExpr list) (range: range) =
  if expr.IsEmpty && range.EndColumn - range.StartColumn <> enclosureWidth then
    Range.shiftStart 0 (enclosureWidth / 2) range
    |> Range.shiftEnd 0 (-enclosureWidth / 2)
    |> fun range -> reportWarn src range "Remove whitespace in empty arraylist"
  else
    ()

/// Checks proper placement of opening bracket after `let` keyword.
let checkOpeningBracketIsInlineWithLet (src: ISourceText) (range: range) =
  if src.GetLineString(range.StartLine - 1).TrimStart().StartsWith "let" then
    reportWarn src range "Move bracket to next line after binding"
  else
    ()

/// Checks proper one element per line in multi-line list/array literals.
let checkSingleElementPerLine src (elemRanges: Range list) =
  elemRanges
  |> List.groupBy (fun range -> range.StartLine)
  |> List.iter (fun (_, ranges) ->
    if ranges.Length > 1 then
      ranges |> List.iter (reportSingleElementPerLineError src)
    else
      ()
  )

let checkEdgeCompFlag (src: ISourceText) (elemRange: range) (range: range) =
  let added =
    if src.GetLineString(elemRange.EndLine).TrimStart().StartsWith "(*" then 2
    else 1
  (Position.mkPos (elemRange.EndLine + added) 0,
    Position.mkPos range.EndLine 0)
  ||> Range.mkRange ""
  |> src.GetSubTextFromRange
  |> fun subStr ->
    subStr.Split([| '\n' |], StringSplitOptions.None)
    |> fun strArr ->
      let flagStartIsWrong =
        (Array.head strArr).TrimStart().StartsWith "#if" |> not
      let flagEndIsWrong = Array.last strArr |> String.IsNullOrEmpty |> not
      if flagEndIsWrong || flagStartIsWrong then
        reportWarn src elemRange "Move element inline with bracket"
      else
        ()

/// Checks proper bracket-element alignment in multi-line list/array literals.
let checkElemIsInlineWithBracket src isArray (range: range) (elemRange: range) =
  let distFstElemToOpeningBracket = if isArray then 3 else 2
  let isOnlyCommentInlineWithBracket =
    (src: ISourceText).GetLineString(range.StartLine - 1).IndexOf "(*"
    - distFstElemToOpeningBracket = range.StartColumn
  if isOnlyCommentInlineWithBracket then elemRange.EndLine - 1
  else elemRange.EndLine
  |> fun endLineOfElem ->
    if (elemRange.StartLine <> range.StartLine
      && not isOnlyCommentInlineWithBracket)
      || endLineOfElem <> range.EndLine then
      try checkEdgeCompFlag src elemRange range
      with _ -> reportWarn src elemRange "Move element inline with bracket"
    else
      ()

/// In single-line, the last element must not be followed by a semicolon.
/// In multi-line, semicolons must not appear at all.
let checkTrailingSeparator src fRange eRange =
  if (fRange: range).StartLine <> fRange.EndLine then
    for line in fRange.StartLine .. fRange.EndLine - 1 do
      let lineString = (src: ISourceText).GetLineString line
      let lastSepaIdx = lineString.LastIndexOf ";"
      if lastSepaIdx + 1 = lineString.Length then
        (Position.mkPos (line + 1) lastSepaIdx,
         Position.mkPos (line + 1) lineString.Length)
        ||> Range.mkRange ""
        |> reportTrailingSeparator src
      else
        ()
  else
    let gap = Range.mkRange "" (eRange: range).End fRange.End
    let lastToClosingBracket = gap |> src.GetSubTextFromRange
    if lastToClosingBracket.Contains ';' then reportTrailingSeparator src gap
    else ()

/// Adjusts the range to exclude comments (e.g., (* ... *)) inside brackets.
/// Useful for spacing checks when comments are present.
let adjustRangeByComment (src: ISourceText) (range: range) (expr: SynExpr) =
  range.StartLine - expr.Range.StartLine
  |> fun startLineDiff ->
    if startLineDiff <> 0 then Range.shiftEnd startLineDiff 0 range else range
    |> fun rangeAdjusted ->
      let endLineString =
        if range.StartLine <> range.EndLine then
          src.GetLineString(range.EndLine - 1)
        else
          src.GetSubTextFromRange range
      endLineString.IndexOf "(*"
      |> fun openCommentIdxAtEndLine ->
        if openCommentIdxAtEndLine <> -1 then
          let closeCommentIdxAtEndLine =
            endLineString.IndexOf "*)"
            |> fun idx -> if idx = -1 then idx else idx + 2
          openCommentIdxAtEndLine - closeCommentIdxAtEndLine - 1
          |> fun amt -> Range.shiftEnd 0 amt rangeAdjusted
        else
          rangeAdjusted

let checkCommon src isArray full elem =
  let distFstElemToOpeningBracket = if isArray then 3 else 2
  checkTrailingSeparator src full elem
  checkBracketSpacing src distFstElemToOpeningBracket elem full

let rec checkSingleLine src = function
  | SynExpr.Sequential _ as expr ->
    collectElemAndOptionalSeparatorRanges [] expr |> checkElementSpacing src
  | SynExpr.IndexRange(expr1 = expr1; opm = opm; range2 = range2) ->
    checkRangeOpSpacing src expr1.Value range2 opm
  | SynExpr.Paren(expr = innerExpr) ->
    checkSingleLine src innerExpr
  | SynExpr.Tuple(exprs = exprs) ->
    exprs |> List.iter (checkSingleLine src)
  | SynExpr.DotGet(expr, dotm, longDotId, _) ->
    FunctionCallConvention.checkDotGetSpacing src expr dotm longDotId
    checkSingleLine src expr
  | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
    checkSingleLine src funcExpr
    checkSingleLine src argExpr
  | SynExpr.YieldOrReturn _
  | SynExpr.YieldOrReturnFrom _
  | SynExpr.Upcast _
  | SynExpr.ArrayOrList _
  | SynExpr.ArrayOrListComputed _
  | SynExpr.InterpolatedString _
  | SynExpr.IfThenElse _
  | SynExpr.Record _
  | SynExpr.LongIdent _
  | SynExpr.ForEach _
  | SynExpr.Const _
  | SynExpr.Ident _
  | SynExpr.App _ -> () (* No need to check string here *)
  | expr -> warn $"[checkSingleLine]TODO: {expr}"

let checkMultiLine src isArray range = function
  | SynExpr.Sequential _ as expr ->
    checkOpeningBracketIsInlineWithLet src range
    collectElemAndOptionalSeparatorRanges [] expr
    |> checkSingleElementPerLine src
    checkElemIsInlineWithBracket src isArray range expr.Range
  | SynExpr.ArrayOrListComputed _
  | SynExpr.Record _
  | SynExpr.App _
  | SynExpr.ForEach _ -> () (* No need to check string here *)
  | expr -> warn $"[checkMultiLine]TODO: {expr}"

let check src isArray (range: Range) expr =
  let rangeAdjusted = adjustRangeByComment src range expr
  checkCommon src isArray rangeAdjusted expr.Range
  if range.StartLine = range.EndLine then checkSingleLine src expr
  else checkMultiLine src isArray rangeAdjusted expr