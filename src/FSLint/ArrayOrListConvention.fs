module B2R2.FSLint.ArrayOrListConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private reportRangeOperatorError src range =
  reportError src range "Spaces required around range operator"

let private reportBracketSpacingError src range =
  reportError src range "Wrong spacing inside brackets"

let private reportSingleElementPerLineError src range =
  reportError src range "Only one element per line allowed"

let rec collectElemAndOptionalSeparatorRanges acc = function
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
    if elemRange.EndColumn <> separatorRange.StartColumn then
      reportError src separatorRange
      <| $"Separator must be attached to the preceding element"
    elif separatorRange.EndColumn = nextElement.StartColumn
      || separatorRange.EndColumn + 1 <> nextElement.StartColumn
    then
      reportError src separatorRange "Exactly 1 space required after separator"
    else ()

let checkBracketCompFlag (src: ISourceText) (elemRange: range) (range: range) =
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
        reportBracketSpacingError src range
      else ()

/// Checks proper spacing inside brackets for list/array literals.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing src distFstElemToOpenBracket (elemRange: range) range =
  let isEndBracketSpacingIncorrect =
    (range: range).EndColumn - distFstElemToOpenBracket <> elemRange.EndColumn
  let isStartBracketSpacingIncorrect =
    range.StartColumn + distFstElemToOpenBracket <> elemRange.StartColumn
  if isStartBracketSpacingIncorrect || isEndBracketSpacingIncorrect then
    try checkBracketCompFlag src elemRange range
    with _ -> reportBracketSpacingError src range
  else ()

/// Checks proper spacing around range operator (..) in list/array literals.
/// Ensures single space before and after '..' (e.g., "1 .. 10" not "1..10").
let checkRangeOpSpacing src fstElem (rangeOfSecondElem: range) (opm: range) =
  match fstElem with
  | SynExpr.Const(range = rangeOfFirstElem) ->
    if opm.StartColumn = rangeOfFirstElem.EndColumn
      || opm.EndColumn = rangeOfSecondElem.StartColumn
    then reportRangeOperatorError src opm
    else ()
  | SynExpr.Ident ident ->
    if opm.StartColumn = ident.idRange.EndColumn
      || opm.EndColumn = rangeOfSecondElem.StartColumn
    then reportRangeOperatorError src opm
    else ()
  | SynExpr.IndexRange(opm = stepOpm
                       range1 = rangeOfFirstElem
                       range2 = stepRange) ->
    if stepOpm.StartColumn = rangeOfFirstElem.EndColumn
      || stepOpm.EndColumn = stepRange.StartColumn
    then reportRangeOperatorError src stepOpm
    elif opm.StartColumn = stepRange.EndColumn
      || opm.EndColumn = rangeOfSecondElem.StartColumn
    then reportRangeOperatorError src opm
    else ()
  | _ -> ()

/// Checks proper spacing in empty list/array literals.
/// Ensures no space inside empty brackets (e.g., "[]" or "[||]" not "[ ]").
let checkEmpty src enclosureWidth (expr: SynExpr list) (range: range) =
  if expr.IsEmpty && range.EndColumn - range.StartColumn <> enclosureWidth then
    reportError src range "Contains Invalid Whitespace"
  else
    ()

/// Checks proper placement of opening bracket after `let` keyword.
let checkOpeningBracketIsInlineWithLet (src: ISourceText) (range: range) =
  if src.GetLineString(range.StartLine - 1).TrimStart().StartsWith "let" then
    reportError src range "Misplaced bracket after binding keyword"
  else
    ()

/// Checks proper one element per line in multi-line list/array literals.
let checkSingleElementPerLine src (elemRanges: Range list) =
  elemRanges
  |> List.groupBy (fun range -> range.StartLine)
  |> List.iter (fun (_, ranges) ->
    if ranges.Length > 1 then
      ranges |> List.iter (reportSingleElementPerLineError src)
    else ()
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
        reportError src elemRange "Bracket-edge element must be inline"
      else ()

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
      with _ -> reportError src elemRange "BracketEdge element must be inline"
    else ()

/// In single-line, the last element must not be followed by a semicolon.
/// In multi-line, semicolons must not appear at all.
let checkTrailingSeparator src isPat distFstElemToOpeningBracket range =
  if (range: range).StartLine <> range.EndLine then
    for line in range.StartLine .. range.EndLine - 1 do
      let lineString = (src: ISourceText).GetLineString line
      if lineString.LastIndexOf ";" + 1 = lineString.Length then
        reportError src range "Contains Invalid Separator"
      else ()
  else
    let distClosingBracketToLastSeparator =
      let lastIdxOfSeparator =
        src.GetSubTextFromRange(range).LastIndexOf ";"
        |> fun idx -> if isPat then idx + range.StartColumn else idx
      range.EndColumn - lastIdxOfSeparator
    if distClosingBracketToLastSeparator < distFstElemToOpeningBracket + 1 then
      reportError src range "Contains Invalid Separator"
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
        else src.GetSubTextFromRange range
      endLineString.IndexOf "(*"
      |> fun openCommentIdxAtEndLine ->
        if openCommentIdxAtEndLine <> -1 then
          let closeCommentIdxAtEndLine =
            endLineString.IndexOf "*)"
            |> fun idx -> if idx = -1 then idx else idx + 2
          openCommentIdxAtEndLine - closeCommentIdxAtEndLine - 1
          |> fun amt -> Range.shiftEnd 0 amt rangeAdjusted
        else rangeAdjusted

let checkCommon src isArray fullRange elemRange =
  let distFstElemToOpeningBracket = if isArray then 3 else 2
  checkTrailingSeparator src false distFstElemToOpeningBracket fullRange
  checkBracketSpacing src distFstElemToOpeningBracket elemRange fullRange

let rec checkSingleLine src = function
  | SynExpr.Sequential _ as expr ->
    collectElemAndOptionalSeparatorRanges [] expr |> checkElementSpacing src
  | SynExpr.IndexRange(expr1 = exprOfFirstElement
                       opm = opm
                       range2 = rangeOfSecondElement) ->
    checkRangeOpSpacing src exprOfFirstElement.Value rangeOfSecondElement opm
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
  | SynExpr.Paren _
  | SynExpr.Tuple _
  | SynExpr.ArrayOrListComputed _
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
  | SynExpr.ArrayOrListComputed _ | SynExpr.Record _
  | SynExpr.App _ | SynExpr.ForEach _ -> () (* No need to check string here *)
  | expr -> warn $"[checkMultiLine]TODO: {expr}"

let check src isArray (range: Range) expr =
  let rangeAdjusted = adjustRangeByComment src range expr
  checkCommon src isArray rangeAdjusted expr.Range
  if range.StartLine = range.EndLine then checkSingleLine src expr
  else checkMultiLine src isArray rangeAdjusted expr