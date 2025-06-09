module B2R2.FSLint.ArrayOrListConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private reportRangeOperatorSpacing (src: ISourceText) line column =
  Console.WriteLine (src.GetLineString (line - 1))
  Console.WriteLine(String.replicate column " " + "^")
  raiseWithError $"Line {line}: Spaces required around range operator"

let private reportBracketSpacing (src: ISourceText) line column =
  Console.WriteLine (src.GetLineString (line - 1))
  Console.WriteLine(String.replicate column " " + "^")
  raiseWithError $"Line {line}: Spaces required inside brackets"

let rec collectSeparatorAndElementRanges acc = function
  | SynExpr.Sequential (expr1 = expr1; expr2 = expr2; trivia = trivia) ->
    collectSeparatorAndElementRanges
      (trivia.SeparatorRange.Value :: expr1.Range :: acc) expr2
  | expr -> expr.Range :: acc |> List.rev

/// Checks proper spacing after semicolons between list/array elements.
/// Ensures exactly one space after each semicolon (e.g., "1; 2; 3").
let checkElementSpacing (src: ISourceText) (elemAndSepRanges: Range list) =
  for i in 0 .. 2 .. elemAndSepRanges.Length - 3 do
    let elementRange = elemAndSepRanges[i]
    let separatorRange = elemAndSepRanges[i + 1]
    let nextElement = elemAndSepRanges[i + 2]
    if elementRange.EndColumn <> separatorRange.StartColumn then
      Console.WriteLine (src.GetLineString (elementRange.StartLine - 1))
      Console.WriteLine (String.replicate separatorRange.StartColumn " " + "^")
      raiseWithError $"Line {elementRange.StartLine}: Separator must be \
      attached to the preceding element"
    elif separatorRange.EndColumn = nextElement.StartColumn then
      Console.WriteLine (src.GetLineString (elementRange.StartLine - 1))
      Console.WriteLine (String.replicate separatorRange.StartColumn " " + "^")
      raiseWithError $"Line {elementRange.StartLine}: Exactly One space \
      required after separator"
    elif separatorRange.EndColumn + 1 <> nextElement.StartColumn then
      let distanceBetweenSeparatorAndNextElement =
        nextElement.StartColumn - separatorRange.EndColumn - 2
      let padToSeparatorPosition =
        String.replicate (elementRange.EndColumn + 1) " "
      Console.WriteLine (src.GetLineString (elementRange.StartLine - 1))
      Console.WriteLine (padToSeparatorPosition + "|" +
      String.replicate distanceBetweenSeparatorAndNextElement "-" + "|")
      raiseWithError $"Line {elementRange.StartLine}: Exactly One space \
      required after separator"
    else ()

/// Checks proper spacing inside brackets for list/array literals.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing src distanceFstElemToOpeningBracket elementRange
  fullRange =
  let isStartBracketSpacingIncorrect =
    (fullRange: range).StartColumn + distanceFstElemToOpeningBracket <>
    (elementRange: range).StartColumn
  let isEndBracketSpacingIncorrect =
    fullRange.EndColumn - distanceFstElemToOpeningBracket <>
    elementRange.EndColumn
  if isStartBracketSpacingIncorrect then
    reportBracketSpacing src fullRange.StartLine fullRange.StartColumn
  elif isEndBracketSpacingIncorrect then
    reportBracketSpacing src fullRange.StartLine elementRange.EndColumn
  else ()

/// Checks proper spacing around range operator (..) in list/array literals.
/// Ensures single space before and after '..' (e.g., "1 .. 10" not "1..10").
let checkRangeOperatorSpacing src exprOfFirstElem (rangeOfSecondElem: range)
  (opm: range) =
  match exprOfFirstElem with
  | Some (SynExpr.Const (range = rangeOfFirstElem)) ->
    if opm.StartColumn = rangeOfFirstElem.EndColumn then
      reportRangeOperatorSpacing src opm.StartLine opm.StartColumn
    elif opm.EndColumn = rangeOfSecondElem.StartColumn then
      reportRangeOperatorSpacing src opm.StartLine (opm.EndColumn - 1)
    else ()
  | Some (SynExpr.IndexRange (opm = stepOpm
                              range1 = rangeOfFirstElem
                              range2 = stepRange)) ->
    if stepOpm.StartColumn = rangeOfFirstElem.EndColumn then
      reportRangeOperatorSpacing src opm.StartLine stepOpm.StartColumn
    elif stepOpm.EndColumn = stepRange.StartColumn then
      reportRangeOperatorSpacing src opm.StartLine (stepOpm.EndColumn - 1)
    elif opm.StartColumn = stepRange.EndColumn then
      reportRangeOperatorSpacing src opm.StartLine opm.StartColumn
    elif opm.EndColumn = rangeOfSecondElem.StartColumn then
      reportRangeOperatorSpacing src opm.StartLine (opm.EndColumn - 1)
    else ()
  | _ -> ()

/// Checks proper spacing in empty list/array literals.
/// Ensures no space inside empty brackets (e.g., "[]" or "[||]" not "[ ]").
let checkEmpty (src: ISourceText) enclosureWidth (expr: list<SynExpr>)
  (fullRange: range) =
  if expr.IsEmpty && fullRange.EndColumn - fullRange.StartColumn <>
    enclosureWidth then
    Console.WriteLine (src.GetLineString (fullRange.StartLine - 1))
    Console.WriteLine (String.replicate fullRange.StartColumn " " + "|" +
    String.replicate (fullRange.EndColumn - fullRange.StartColumn - 2) "-" +
    "|")
    raiseWithError $"Line {fullRange.StartLine} Contains Invalid Whitespace"
  else ()

let checkSingleLine src distanceFstElemToOpeningBracket range = function
  | SynExpr.Const (range = innerRange) ->
    checkBracketSpacing src distanceFstElemToOpeningBracket innerRange range
  | SynExpr.Sequential (range = innerRange) as expr ->
    checkBracketSpacing src distanceFstElemToOpeningBracket innerRange range
    collectSeparatorAndElementRanges [] expr |> checkElementSpacing src
  | SynExpr.IndexRange (expr1 = exprOfFirstElement
                        opm = opm
                        range2 = rangeOfSecondElement
                        range = innerRange) ->
    checkBracketSpacing src distanceFstElemToOpeningBracket innerRange range
    checkRangeOperatorSpacing src exprOfFirstElement rangeOfSecondElement opm
  | expr -> warn $"TODO: {expr}"

let check src isArray (range: range) expr =
  // TODO: ISourceText -> semicolon check
  if range.StartLine = range.EndLine then
    let distanceFstElemToOpeningBracket = if isArray then 3 else 2
    checkSingleLine src distanceFstElemToOpeningBracket range expr
  else
    warn $"TODO: {expr}"