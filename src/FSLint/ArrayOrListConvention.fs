module B2R2.FSLint.ArrayOrListConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let rec collectSeparatorAndElementRanges acc = function
  | SynExpr.Sequential (expr1 = expr1; expr2 = expr2; trivia = trivia) ->
    collectSeparatorAndElementRanges
      (trivia.SeparatorRange.Value :: expr1.Range :: acc) expr2
  | expr -> expr.Range :: acc |> List.rev

/// Checks proper spacing after semicolons between list/array elements.
/// Ensures exactly one space after each semicolon (e.g., "1; 2; 3").
let checkElementSpacing (elementAndSeparatorRanges: Range list) =
  for i in 0 .. 2 .. elementAndSeparatorRanges.Length - 3 do
    let elementRange = elementAndSeparatorRanges[i]
    let separatorRange = elementAndSeparatorRanges[i + 1]
    let nextElement = elementAndSeparatorRanges[i + 2]
    if elementRange.EndColumn <> separatorRange.StartColumn ||
      separatorRange.EndColumn + 1 <> nextElement.StartColumn then
      raiseWithError $"Line {elementRange.StartLine}: Separator Error"
    else ()

/// Checks proper spacing inside brackets for list/array literals.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing distanceFstElemToOpeningBracket elementRange fullRange =
  let isStartBracketSpacingIncorrect =
    (fullRange: range).StartColumn + distanceFstElemToOpeningBracket <>
    (elementRange: range).StartColumn
  let isEndBracketSpacingIncorrect =
    fullRange.EndColumn - distanceFstElemToOpeningBracket <>
    elementRange.EndColumn
  if isStartBracketSpacingIncorrect || isEndBracketSpacingIncorrect then
    raiseWithError $"Line {fullRange.StartLine} space indentation wrong"
  else ()

/// Checks proper spacing around range operator (..) in list/array literals.
/// Ensures single space before and after '..' (e.g., "1 .. 10" not "1..10").
let checkRangeOperatorSpacing exprOfFirstElem rangeOfSecondElem (opm: range) =
  match exprOfFirstElem with
  | Some (SynExpr.Const (range = rangeOfFirstElem)) ->
    if opm.StartColumn = rangeOfFirstElem.EndColumn ||
      opm.EndColumn = (rangeOfSecondElem: range).StartColumn then
      raiseWithError $"Line {opm.StartLine} '..' spacing wrong"
    else ()
  | Some (SynExpr.IndexRange (opm = stepOpm
                              range1 = rangeOfFirstElem
                              range2 = stepRange)) ->
    let stepOpmStart = stepOpm.StartColumn = rangeOfFirstElem.EndColumn
    let stepOpmEnd = stepOpm.EndColumn = stepRange.StartColumn
    let opmStart = opm.StartColumn = stepRange.EndColumn
    let opmEnd = opm.EndColumn = rangeOfSecondElem.StartColumn
    if stepOpmStart || stepOpmEnd || opmStart || opmEnd then
      raiseWithError $"Line {opm.StartLine} '..' spacing wrong"
    else ()
  | _ -> ()

/// Checks proper spacing in empty list/array literals.
/// Ensures no space inside empty brackets (e.g., "[]" or "[||]" not "[ ]").
let checkEmpty enclosureWidth (expr: list<SynExpr>)
  (fullRange: range) =
  if expr.IsEmpty && fullRange.EndColumn - fullRange.StartColumn <>
    enclosureWidth then
    raiseWithError $"Line {fullRange.StartLine} Contains Invalid Whitespace"
  else ()

let checkSingleLine distanceFstElemToOpeningBracket range = function
  | SynExpr.Const (range = innerRange) ->
    checkBracketSpacing distanceFstElemToOpeningBracket innerRange range
  | SynExpr.Sequential (range = innerRange) as expr ->
    checkBracketSpacing distanceFstElemToOpeningBracket innerRange range
    collectSeparatorAndElementRanges [] expr |> checkElementSpacing
  | SynExpr.IndexRange (expr1 = exprOfFirstElement
                        opm = opm
                        range2 = rangeOfSecondElement
                        range = innerRange) ->
    checkBracketSpacing distanceFstElemToOpeningBracket innerRange range
    checkRangeOperatorSpacing exprOfFirstElement rangeOfSecondElement opm
  | expr -> failwith $"TODO: {expr}"

let check isArray (range: range) expr =
  // TODO: ISourceText -> semicolon check
  if range.StartLine = range.EndLine then
    let distanceFstElemToOpeningBracket = if isArray then 3 else 2
    checkSingleLine distanceFstElemToOpeningBracket range expr
  else
    failwith $"TODO: {expr}"