namespace B2R2.FSLint

module ArrayOrListConvention =
  open FSharp.Compiler.Text
  open FSharp.Compiler.Syntax
  open FSharp.Compiler.SyntaxTrivia

  let rec collectRanges = function
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2; trivia = trivia) ->
      [ expr1.Range, trivia.SeparatorRange.Value ] @ collectRanges expr2
    | expr -> [ expr.Range, Range.Zero ]

  /// Checks proper spacing after semicolons between list/array elements.
  /// Ensures exactly one space after each semicolon (e.g., "1; 2; 3").
  and checkElementSpacing expr =
    let elementAndSeparatorRange = collectRanges expr
    for i = 0 to List.length elementAndSeparatorRange - 2 do
      let elementRange, separatorRange = elementAndSeparatorRange[i]
      let nextElement = elementAndSeparatorRange[i + 1] |> fst
      if elementRange.EndColumn <> separatorRange.StartColumn ||
        separatorRange.EndColumn + 1 <> nextElement.StartColumn then
        raiseWithError
          $"Line {elementRange.StartLine}: Separator Error"

  /// Checks proper spacing inside brackets for list/array literals.
  /// Ensures single space after opening and before closing brackets.
  let checkBracketSpacing arity (elementRange: range) (fullRange: range) =
    let startPosSpace =
      fullRange.StartColumn + arity <> elementRange.StartColumn
    let endPosSpace = fullRange.EndColumn - arity <> elementRange.EndColumn
    if startPosSpace || endPosSpace then
      raiseWithError $"Line {fullRange.StartLine} space indentation wrong"

  /// Checks proper spacing around range operator (..) in list/array literals.
  /// Ensures single space before and after '..' (e.g., "1 .. 10" not "1..10").
  let checkRangeOperatorSpacing (range2: range) (opm: range) = function
    | Some (SynExpr.Const (range = range1)) ->
      if opm.StartColumn = range1.EndColumn ||
        opm.EndColumn = range2.StartColumn then
        raiseWithError $"Line {opm.StartLine} '..' spacing wrong"
    | Some (SynExpr.IndexRange (opm = stepOpm
                                range1 = startVal
                                range2 = stepValRange)) ->
      let stepOpmStart = stepOpm.StartColumn = startVal.EndColumn
      let stepOpmEnd = stepOpm.EndColumn = stepValRange.StartColumn
      let opmStart = opm.StartColumn = stepValRange.EndColumn
      let opmEnd = opm.EndColumn = range2.StartColumn
      if stepOpmStart || stepOpmEnd || opmStart || opmEnd then
        raiseWithError $"Line {opm.StartLine} '..' spacing wrong"
    | _ -> ()

  /// Checks proper spacing in empty list/array literals.
  /// Ensures no space inside empty brackets (e.g., "[]" or "[||]" not "[ ]").
  let checkEmptyArrayOrList arity (expr: list<SynExpr>) (range: range) =
    if expr.IsEmpty && range.EndColumn - range.StartColumn <> arity then
      raiseWithError $"Line {range.StartLine} Contains Invalid Whitespace"

  let checkSingleLine arity range = function
    | SynExpr.Const (range = insideRange) ->
      checkBracketSpacing arity insideRange range
    | SynExpr.Sequential (range = insideRange) as expr ->
      checkBracketSpacing arity insideRange range
      checkElementSpacing expr
    | SynExpr.IndexRange (expr1 = expr1
                          opm = opm
                          range2 = range2
                          range = insideRange) ->
      checkBracketSpacing arity insideRange range
      checkRangeOperatorSpacing range2 opm expr1
    | expr -> failwith $"TODO: {expr}"

  let check isArray (range: range) expr =
    if range.StartLine = range.EndLine then
      let arity = if isArray then 3 else 2
      checkSingleLine arity range expr
    else
      failwith $"TODO: {expr}"