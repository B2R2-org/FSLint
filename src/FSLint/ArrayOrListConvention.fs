module B2R2.FSLint.ArrayOrListConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private reportRangeOperatorError src range =
  reportError src range "Spaces required around range operator"

let private reportBracketSpacingError src range =
  reportError src range "Wrong spacing inside brackets"

let private reportInfixError src range =
  reportError src range "There must be a space before and after the infix"

let private reportSingleElementPerLineError src range =
  reportError src range "Only one element per line allowed"

let rec collectElemAndOptionalSeparatorRanges acc = function
  | SynExpr.Sequential (expr1 = expr1; expr2 = expr2; trivia = trivia) ->
    trivia.SeparatorRange
    |> Option.map (fun sep -> sep :: expr1.Range :: acc)
    |> Option.defaultValue (expr1.Range :: acc)
    |> fun acc -> collectElemAndOptionalSeparatorRanges acc expr2
  | expr -> expr.Range :: acc |> List.rev

let collectElemAndOptionalSeparatorRangesInPat (src: ISourceText) elementPats =
  let separatorRanges =
    (elementPats: SynPat list)
    |> List.map (fun pat -> pat.Range)
    |> List.pairwise
    |> List.map (fun (fstRange, sndRange) ->
      let betweenElementsRange = Range.mkRange "" fstRange.End sndRange.Start
      let betweenElementsText = src.GetSubTextFromRange betweenElementsRange
      let semicolonIndex = betweenElementsText.IndexOf ";" + fstRange.EndColumn
      let startPos = Position.mkPos fstRange.StartLine semicolonIndex
      let endPos = Position.mkPos fstRange.StartLine (semicolonIndex + 1)
      Range.mkRange "" startPos endPos)
  let elementRanges = List.map (fun (pat: SynPat) -> pat.Range) elementPats
  let rec interleave elements separators acc =
    match elements, separators with
    | [], [] -> List.rev acc
    | [ elem ], [] -> List.rev (elem :: acc)
    | elem :: restElems, sep :: restSeps ->
      interleave restElems restSeps (sep :: elem :: acc)
    | _ -> reportError src elementPats.Head.Range "Pattern ParsingFailure"
  interleave elementRanges separatorRanges []

let rec collectBracketInfoInAppExpr = function
  | SynExpr.App (funcExpr = funcExpr; argExpr = argExpr) ->
    if argExpr.IsArrayOrListComputed
    then argExpr :: collectBracketInfoInAppExpr funcExpr
    else collectBracketInfoInAppExpr argExpr
  | _ -> []

let collectCastSymbolRangeFromSrc (src: ISourceText) expr =
  match expr with
  | SynExpr.Upcast (expr = innerExpr; targetType = targetType) ->
    let symbWidthIncludeSpace =
      targetType.Range.StartColumn - innerExpr.Range.EndColumn - 1
    src.GetLineString(innerExpr.Range.StartLine - 1)
      .Substring(innerExpr.Range.EndColumn + 1).Remove(symbWidthIncludeSpace)
      .Trim ()
    |> fun symbolStr ->
      let symbolStart =
        src.GetLineString(innerExpr.Range.StartLine - 1).IndexOf symbolStr
      (Position.mkPos innerExpr.Range.StartLine symbolStart,
      Position.mkPos innerExpr.Range.StartLine (symbolStart + symbolStr.Length))
      ||> Range.mkRange ""
  | _ -> Range.Zero

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

/// Checks proper spacing inside brackets for list/array literals.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing src distFstElemToOpenBracket (elemRange: range) range =
  let isEndBracketSpacingIncorrect =
    (range: range).EndColumn - distFstElemToOpenBracket <> elemRange.EndColumn
  let isStartBracketSpacingIncorrect =
    range.StartColumn + distFstElemToOpenBracket <> elemRange.StartColumn
  if isStartBracketSpacingIncorrect || isEndBracketSpacingIncorrect then
    reportBracketSpacingError src range
  else ()

/// Checks proper spacing around range operator (..) in list/array literals.
/// Ensures single space before and after '..' (e.g., "1 .. 10" not "1..10").
let checkRangeOpSpacing src fstElem (rangeOfSecondElem: range) (opm: range) =
  match fstElem with
  | SynExpr.Const (range = rangeOfFirstElem) ->
    if opm.StartColumn = rangeOfFirstElem.EndColumn
      || opm.EndColumn = rangeOfSecondElem.StartColumn
    then reportRangeOperatorError src opm
    else ()
  | SynExpr.Ident ident ->
    if opm.StartColumn = ident.idRange.EndColumn
      || opm.EndColumn = rangeOfSecondElem.StartColumn
    then reportRangeOperatorError src opm
    else ()
  | SynExpr.IndexRange (opm = stepOpm
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
  else ()

/// Checks proper placement of opening bracket after `let` keyword.
let checkOpeningBracketIsInlineWithLet (src: ISourceText) (range: range) =
  if src.GetLineString(range.StartLine - 1).TrimStart().StartsWith "let" then
    reportError src range "Misplaced bracket after binding keyword"
  else ()

let private ensureInfixSpacing src subFuncRange subArgRange argRange =
  let spaceBetweenFuncAndArg =
    (subFuncRange: range).EndColumn - subFuncRange.StartColumn + 1
  (* Check before operator spacing. *)
  if subFuncRange.EndColumn - (subArgRange: range).EndColumn
    <> spaceBetweenFuncAndArg then
    reportInfixError src subFuncRange
  else ()
  (* Check after operator spacing. *)
  if (argRange: range).StartColumn - subFuncRange.EndColumn <> 1 then
    reportInfixError src argRange
  else ()

/// Check proper infix spacing in list/array literals.
/// Ensures single space before and after infix (e.g., "a + b" not "a+b").
let rec checkInfixSpacing src isInfix (funcExpr: SynExpr) (argExpr: SynExpr) =
  match funcExpr with
  | SynExpr.App (isInfix = isInfixOfInner
                 funcExpr = subFuncExpr
                 argExpr = subArgExpr) ->
    if isInfix || isInfixOfInner then
      ensureInfixSpacing src subFuncExpr.Range subArgExpr.Range argExpr.Range
    else ()
    match subArgExpr with
    | SynExpr.App (isInfix = isInfix
                   funcExpr = subSubFuncExpr
                   argExpr = subSubArgExpr) ->
      checkInfixSpacing src isInfix subSubFuncExpr subSubArgExpr
    | _ -> () (* Skip further checks if not a function application. *)
  | SynExpr.LongIdent _ -> () (* Normally, does not affect overall behavior. *)
  | _ -> warn $"[checkInfixSpacing]TODO: {funcExpr}"

let private ensureFunAppSpacing src (funcRange: range) (argRange: range) =
  if argRange.StartColumn - funcRange.EndColumn <> 1 then
    reportError src argRange "Func app must be separated by a single space."
  else ()

/// Check proper spacing in function applications.
/// Ensures single space between each applied element
/// (e.g., "fn 1 2", not "fn  1  2").
let rec checkFuncAppSpacing src (funcExpr: SynExpr) (argExpr: SynExpr) =
  match funcExpr with
  | SynExpr.App (funcExpr = subFuncExpr; argExpr = subArgExpr) ->
    ensureFunAppSpacing src funcExpr.Range argExpr.Range
    checkFuncAppSpacing src subFuncExpr subArgExpr
  | SynExpr.Ident _ | SynExpr.LongIdent _ ->
    ensureFunAppSpacing src funcExpr.Range argExpr.Range
  | _ -> warn $"[checkFuncAppSpacing]TODO: {funcExpr}"

let checkFuncApp src flag (funcExpr: SynExpr) (argExpr: SynExpr) =
  match funcExpr with
  | SynExpr.App (isInfix = isInfix) ->
    if isInfix then checkInfixSpacing src true funcExpr argExpr
    else checkFuncAppSpacing src funcExpr argExpr
  | SynExpr.Ident _ | SynExpr.LongIdent _ when flag <> ExprAtomicFlag.Atomic ->
    checkFuncAppSpacing src funcExpr argExpr
  | SynExpr.LongIdent _ -> ()
  | expr -> warn $"[checkFuncApp]TODO: {expr}"

/// Checks proper one element per line in multi-line list/array literals.
let checkSingleElementPerLine src (elemRanges: Range list) =
  elemRanges
  |> List.groupBy (fun range -> range.StartLine)
  |> List.iter (fun (_, ranges) ->
    if ranges.Length > 1 then
      ranges |> List.iter (reportSingleElementPerLineError src)
    else ()
  )

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
      reportError src elemRange "Bracket-edge element must be inline"
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

/// Checks cons operator in the context is properly surrounded by single spaces.
let checkConsOperatorSpacing src lhsRange rhsRange (colonRange: range) =
  if (lhsRange: range).EndColumn + 1 <> colonRange.StartColumn
    || (rhsRange: range).StartColumn - 1 <>  colonRange.EndColumn then
    reportError src colonRange "Cons must be surrounded by single spaces"
  else ()

/// Checks that there is no space between the bracket/element for indexers.
/// Ensures no space between bracket and element in indexer expressions.
let checkSpacingInIndexedProperty src expr =
  collectBracketInfoInAppExpr expr
  |> List.iter (fun computed ->
    match computed with
    | SynExpr.ArrayOrListComputed (expr = innerExpr; range = range) ->
      if range.StartColumn + 1 <> innerExpr.Range.StartColumn
        || range.EndColumn - 1 <> innerExpr.Range.EndColumn then
        reportError src range "No space allowed in indexer"
      else ()
      match innerExpr with
      | SynExpr.IndexRange (opm = opm; expr1 = expr1; expr2 = expr2) ->
        match expr1, expr2 with
        | Some e1, Some e2 when
          e1.Range.EndColumn <> opm.StartColumn
          || e2.Range.StartColumn <> opm.EndColumn ->
          reportError src opm "No space allowed in indexer"
        | Some e1, None when e1.Range.EndColumn <> opm.StartColumn ->
          reportError src opm "No space allowed in indexer"
        | None, Some e2 when e2.Range.StartColumn <> opm.EndColumn ->
          reportError src opm "No space allowed in indexer"
        | _ -> ()
      | _ -> ()
    | _ -> ()
  )

let checkCommaSpacingOfTuple src exprs commaRanges =
  exprs
  |> List.pairwise
  |> List.zip commaRanges
  |> List.iter (fun (commaRange, (fstElem, sndElem)) ->
    if (fstElem: SynExpr).Range.EndColumn <> (commaRange: range).StartColumn
    then
      reportError src commaRange "No space allowed before comma"
    elif sndElem.Range.StartColumn - 1 <> commaRange.EndColumn then
      reportError src commaRange "Need to space after comma"
    else ()
  )

/// Checks the spacing around the upcast operator (:>) in infix expressions.
let checkInfixSpacingFromUpcast src (innerRange: range) targetType symbolRange =
  match targetType with
  | SynType.App (range = castRange) ->
    if innerRange.EndColumn + 1 <> (symbolRange: range).StartColumn
      || castRange.StartColumn - 1 <> symbolRange.EndColumn
    then reportInfixError src symbolRange
    else ()
  | _ -> ()

/// Adjusts the range to exclude comments (e.g., (* ... *)) inside brackets.
/// Useful for spacing checks when comments are present.
let adjustRangeByComment (src: ISourceText) (range: range) (expr: SynExpr) =
  range.StartLine - expr.Range.StartLine
  |> fun startLineDiff ->
    if startLineDiff <> 0 then Range.shiftEnd startLineDiff 0 range else range
    |> fun rangeAdjusted ->
      let endLineString =
        if range.StartLine <> range.EndLine then
          src.GetLineString (range.EndLine - 1)
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

let rec checkPattern (src: ISourceText) = function
  | SynPat.ArrayOrList (isArray, elementPats, range) ->
    if elementPats.IsEmpty then
      let enclosureWidth = if isArray then 4 else 2
      if range.EndColumn - range.StartColumn <> enclosureWidth then
        reportError src range "Contains Invalid Whitespace"
      else ()
    else
      elementPats
      |> List.map (fun pat -> pat.Range)
      |> List.reduce Range.unionRanges
      |> checkCommon src isArray range
      collectElemAndOptionalSeparatorRangesInPat src elementPats
      |> checkElementSpacing src
  | SynPat.ListCons (lhsPat = lhsPat; rhsPat = rhsPat; trivia = triv) ->
    checkConsOperatorSpacing src lhsPat.Range rhsPat.Range triv.ColonColonRange
    checkPattern src lhsPat
    checkPattern src rhsPat
  | _ -> () (* no need to check this *)

let rec checkSingleLine src = function
  | SynExpr.Sequential _ as expr ->
    collectElemAndOptionalSeparatorRanges [] expr |> checkElementSpacing src
  | SynExpr.IndexRange (expr1 = exprOfFirstElement
                        opm = opm
                        range2 = rangeOfSecondElement) ->
    checkRangeOpSpacing src exprOfFirstElement.Value rangeOfSecondElement opm
  | SynExpr.App (flag = flag; funcExpr = funExpr; argExpr = argExpr) ->
    checkFuncApp src flag funExpr argExpr
  | SynExpr.YieldOrReturn (expr = expr)
  | SynExpr.YieldOrReturnFrom (expr = expr) ->
    checkSingleLine src expr
  | SynExpr.ForEach (pat = pat; enumExpr = enumExpr; bodyExpr = bodyExpr) ->
    checkPattern src pat
    checkSingleLine src enumExpr
    checkSingleLine src bodyExpr
  | SynExpr.Paren (expr = expr) -> checkSingleLine src expr
  | SynExpr.Tuple (exprs = exprs; commaRanges = commaRanges) ->
    checkCommaSpacingOfTuple src exprs commaRanges
    List.iter (checkSingleLine src) exprs
  | SynExpr.Upcast (expr = innerExpr; targetType = targetType) as expr ->
    collectCastSymbolRangeFromSrc src expr
    |> checkInfixSpacingFromUpcast src innerExpr.Range targetType
    checkSingleLine src innerExpr (* TODO: targetType *)
  | SynExpr.InterpolatedString _ (* No need to check string here *)
  | SynExpr.Const _
  | SynExpr.Ident _
  | SynExpr.LongIdent _ -> ()
  | expr -> warn $"[checkSingleLine]TODO: {expr}"

let checkMultiLine src isArray range = function
  | SynExpr.Sequential _ as expr ->
    checkOpeningBracketIsInlineWithLet src range
    collectElemAndOptionalSeparatorRanges [] expr
    |> checkSingleElementPerLine src
    checkElemIsInlineWithBracket src isArray range expr.Range
  | SynExpr.ForEach (pat = pat; enumExpr = enumExpr; bodyExpr = bodyExpr) ->
    checkPattern src pat
    checkSingleLine src enumExpr
    checkSingleLine src bodyExpr
  | expr -> warn $"[checkMultiLine]TODO: {expr}"

let check src isArray (range: Range) expr =
  let rangeAdjusted = adjustRangeByComment src range expr
  checkCommon src isArray rangeAdjusted expr.Range
  if range.StartLine = range.EndLine then
    checkSingleLine src expr
  else
    checkMultiLine src isArray rangeAdjusted expr