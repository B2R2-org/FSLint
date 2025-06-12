module B2R2.FSLint.ArrayOrListConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private reportRangeOperatorError src range =
  reportError src range "Spaces required around range operator"

let private reportBracketSpacingError src range =
  reportError src range "Wrong spacing inside brackets"

let private reportOperatorError src range =
  reportError src range "There must be a space before and after the operator"

let rec collectSeparatorAndElementRanges acc = function
  | SynExpr.Sequential (expr1 = expr1; expr2 = expr2; trivia = trivia) ->
    collectSeparatorAndElementRanges
      (trivia.SeparatorRange.Value :: expr1.Range :: acc) expr2
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

/// Checks proper spacing inside brackets for list/array literals.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing src distFstElemToOpeningBracket elemRange fullRange =
  let isStartBracketSpacingIncorrect =
    (fullRange: range).StartColumn + distFstElemToOpeningBracket <>
    (elemRange: range).StartColumn
  let isEndBracketSpacingIncorrect =
    fullRange.EndColumn - distFstElemToOpeningBracket <> elemRange.EndColumn
  if isStartBracketSpacingIncorrect || isEndBracketSpacingIncorrect then
    reportBracketSpacingError src fullRange
  else ()

/// Checks proper spacing around range operator (..) in list/array literals.
/// Ensures single space before and after '..' (e.g., "1 .. 10" not "1..10").
let checkRangeOpSpacing src fstElem (rangeOfSecondElem: range) (opm: range) =
  match fstElem with
  | Some (SynExpr.Const (range = rangeOfFirstElem)) ->
    if opm.StartColumn = rangeOfFirstElem.EndColumn
      || opm.EndColumn = rangeOfSecondElem.StartColumn
    then reportRangeOperatorError src opm
    else ()
  | Some (SynExpr.IndexRange (opm = stepOpm
                              range1 = rangeOfFirstElem
                              range2 = stepRange)) ->
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
let checkEmpty src enclosureWidth (expr: list<SynExpr>) (fullRange: range) =
  let fullWidth = fullRange.EndColumn - fullRange.StartColumn
  if expr.IsEmpty && fullWidth <> enclosureWidth then
    reportError src fullRange "Contains Invalid Whitespace"
  else ()

/// Check proper operator spacing in list/array literals.
/// Ensures single space before and after operator (e.g., "a + b" not "a+b").
let rec checkOperatorSpacing src (funcExpr: SynExpr) (argExpr: SynExpr) =
  match funcExpr with
  | SynExpr.App (funcExpr = fExpr; argExpr = aExpr) ->
    match aExpr with
    | SynExpr.App (funcExpr = fExpr; argExpr = aExpr) ->
      checkOperatorSpacing src fExpr aExpr
    | SynExpr.LongIdent _ ->
      let funcRange = fExpr.Range
      let argRange = aExpr.Range
      if funcRange.EndColumn - argRange.EndColumn <> 2 then
        reportOperatorError src funcRange
      else ()
      match argExpr with
      | SynExpr.App (range = range) ->
        if range.StartColumn - funcRange.EndColumn <> 1 then
          reportOperatorError src argExpr.Range
        else ()
      | _ -> ()
    | _ -> ()
  | _ -> ()

let checkSingleLine src distFstElemToOpeningBracket range = function
  | SynExpr.Const (range = innerRange) ->
    checkBracketSpacing src distFstElemToOpeningBracket innerRange range
  | SynExpr.Sequential (range = innerRange) as expr ->
    checkBracketSpacing src distFstElemToOpeningBracket innerRange range
    collectSeparatorAndElementRanges [] expr |> checkElementSpacing src
  | SynExpr.IndexRange (expr1 = exprOfFirstElement
                        opm = opm
                        range2 = rangeOfSecondElement
                        range = innerRange) ->
    checkBracketSpacing src distFstElemToOpeningBracket innerRange range
    checkRangeOpSpacing src exprOfFirstElement rangeOfSecondElement opm
  | SynExpr.App (flag = flag
                 isInfix = isInfix
                 funcExpr = funExpr
                 argExpr = argExpr
                 range = innerRange) as expr ->
    printfn "%A" expr
    printfn "Infix: %b\nFlag: %A\nFunc: %A\nArg: %A\nRange: %A"
      isInfix flag funExpr argExpr range
    printfn "%A" <| src.GetLineString (range.StartLine - 1)
    checkBracketSpacing src distFstElemToOpeningBracket innerRange range
    checkOperatorSpacing src funExpr argExpr
  | expr -> warn $"TODO: {expr}"

let check src isArray (range: range) expr =
  if range.StartLine = range.EndLine then
    let distFstElemToOpeningBracket = if isArray then 3 else 2
    checkSingleLine src distFstElemToOpeningBracket range expr
  else
    warn $"TODO: {expr}"