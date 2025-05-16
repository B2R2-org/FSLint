namespace B2R2.FSLint

module LiteralConvention =
  open FSharp.Compiler.Text
  open FSharp.Compiler.Syntax
  open FSharp.Compiler.SyntaxTrivia

  let rec extractConstants = function
    | SynExpr.Const (range = range) -> [ range ]
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2) ->
      extractConstants expr1 @ extractConstants expr2
    | _ -> []

  /// Checks proper spacing after semicolons between list/array elements.
  /// Ensures exactly one space after each semicolon (e.g., "1; 2; 3").
  and checkElementSpacing expr1 expr2 =
    extractConstants expr1 @ extractConstants expr2
    |> List.pairwise
    |> List.iter (fun (range1, range2) ->
      let spacing = range2.StartColumn - range1.EndColumn
      if spacing > 2 then
        raiseWithError
          $"Line {range1.StartLine} Column {range1.EndColumn}: Too much spacing"
      elif spacing = 1 then
        raiseWithError
          $"Line {range1.StartLine} Column {range1.EndColumn}: No spacing")

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
  let checkRangeOperatorSpacing (opm: range) (range1: range) (range2: range) =
    let opmStartPos = opm.StartColumn
    let opmEndPos = opm.EndColumn
    let fstElemEndPos = range1.EndColumn
    let sndElemStartPos = range2.StartColumn
    if opmStartPos = fstElemEndPos || opmEndPos = sndElemStartPos then
      raiseWithError $"Line {opm.StartLine} '..' no spaces"

  /// Checks proper spacing in empty list/array literals.
  /// Ensures no space inside empty brackets (e.g., "[]" or "[||]" not "[ ]").
  let checkEmptyLiteral isArray (expr: list<SynExpr>) (range: range) =
    let arity = if isArray then 4 else 2
    if expr.IsEmpty && range.EndColumn - range.StartColumn <> arity then
      raiseWithError $"Line {range.StartLine} Contains Invalid Whitespace"

  /// Checks for unnecessary line breaks in single-line literals.
  /// Detects when literal could fit on same line as binding within 80 chars.
  let checkAvoidableLineBreak (binding: SynBindingTrivia) (range: range) =
    let bindingTrivia = binding.EqualsRange.Value
    if bindingTrivia.StartLine = range.StartLine then ()
    elif bindingTrivia.EndColumn + range.EndColumn <= 81 then
      raiseWithError $"Redundant line break at {bindingTrivia.StartLine}"

  let checkSingleExpr isArray range = function
    | SynExpr.Const (range = insideRange) ->
      let arity = if isArray then 3 else 2
      checkBracketSpacing arity insideRange range
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2; range = insideRange) ->
      let arity = if isArray then 3 else 2
      checkBracketSpacing arity insideRange range
      checkElementSpacing expr1 expr2
    | SynExpr.IndexRange
      ( opm = opm
        range1 = range1
        range2 = range2
        range = insideRange ) ->
      let arity = if isArray then 3 else 2
      checkBracketSpacing arity insideRange range
      checkRangeOperatorSpacing opm range1 range2
    | SynExpr.ArrayOrList (isArray = isArray
                           exprs = innerExprs
                           range = insideRange) ->
      checkEmptyLiteral isArray innerExprs insideRange
    | SynExpr.Null (range = insideRange) ->
      checkEmptyLiteral isArray [] insideRange
    | expr -> failwith $"TODO: {expr}"

  let rec calculateNestingDepth (outerRange: range) = function
    | SynExpr.ArrayOrListComputed (expr = expr; range = range) ->
      1 + calculateNestingDepth range expr
    | SynExpr.ArrayOrList (exprs = []) -> 1
    | SynExpr.ArrayOrList (exprs = exprs) ->
      exprs
      |> List.map (calculateNestingDepth outerRange)
      |> List.max
      |> fun depth -> depth + 1
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2) ->
      max (calculateNestingDepth outerRange expr1)
        (calculateNestingDepth outerRange expr2)
    | _ -> 0

  and collectNestedOuterStructures expr =
    match expr with
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2) ->
      collectNestedOuterStructures expr1 @ collectNestedOuterStructures expr2
    | SynExpr.ArrayOrListComputed (isArray = isArray
                                   expr = innerExpr
                                   range = innerRange) ->
      (innerRange, isArray) :: collectNestedOuterStructures innerExpr
    | SynExpr.ArrayOrList (isArray = isArray
                           exprs = innerExprs
                           range = innerRange) ->
      (innerRange, isArray) ::
      List.collect collectNestedOuterStructures innerExprs
    | _ -> []

  and checkNestedBracketBoundarySpacing isArray expr (range: range) =
    let nLevel = calculateNestingDepth range expr + 1
    let ranges =
      [ range ] @ (collectNestedOuterStructures expr |> List.map fst)
    let prefixs =
      ranges
      |> List.map (fun range -> range.Start.Column)
      |> List.sort
      |> List.take nLevel
      |> List.pairwise
    let suffixs =
      ranges
      |> List.map (fun range -> range.End.Column)
      |> List.sortDescending
      |> List.take nLevel
      |> List.pairwise
    let arrays =
      [ isArray ] @ (collectNestedOuterStructures expr |> List.map snd)
      |> List.take (nLevel - 1)
    List.zip3 arrays prefixs suffixs
    |> List.iter (fun (isArray, prefix, suffix) ->
      let arity = if isArray then 3 else 2
      let checkPrefix = fst prefix + arity <> snd prefix
      let checkSuffix = fst suffix - arity <> snd suffix
      if checkPrefix || checkSuffix then
        raiseWithError
          $"Line {range.StartLine} outer containers have internal spacing")

  let rec findInnerLiterals expr =
    match expr with
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2) ->
      findInnerLiterals expr1 @ findInnerLiterals expr2
    | SynExpr.ArrayOrListComputed (isArray = isArray
                                   expr = innerExpr
                                   range = range) ->
      if isLeafLiteral innerExpr then [ isArray, innerExpr, range ]
      else processComputedLiteral isArray innerExpr range
    | SynExpr.ArrayOrList (isArray = isArray
                           exprs = innerExprs
                           range = range) as exprs ->
      if List.isEmpty innerExprs then [ isArray, exprs, range ]
      else processComputedLiteral isArray exprs range
    | _ -> []

  and isLeafLiteral = function
    | SynExpr.Sequential (_, _, SynExpr.Const _, SynExpr.Const _, _, _)
    | SynExpr.IndexRange _
    | SynExpr.Const _ -> true
    | SynExpr.Sequential (_, _, expr1, expr2, _, _) ->
      match expr1, expr2 with
      | SynExpr.ArrayOrListComputed _, _
      | _, SynExpr.ArrayOrListComputed _ -> false
      | _ -> isLeafLiteral expr1 && isLeafLiteral expr2
    | SynExpr.ArrayOrListComputed (_, innerExpr, _) ->
      match innerExpr with
      | SynExpr.Sequential (_, _, SynExpr.Const _, SynExpr.Const _, _, _)
      | SynExpr.Const _
      | SynExpr.IndexRange _ -> true
      | _ -> false
    | SynExpr.App _ -> true
    | expr -> printfn $"This need to handle {expr}"; false

  and processExprForLiterals expr =
    match expr with
    | SynExpr.ArrayOrListComputed (isArray = isArray
                                   expr = innerExpr
                                   range = range) ->
      if isLeafLiteral innerExpr then [ isArray, innerExpr, range ]
      else findInnerLiterals expr
    | _ -> findInnerLiterals expr

  and processComputedLiteral isArray innerExpr range =
    match innerExpr with
    | SynExpr.IndexRange _ -> [ isArray, innerExpr, range ]
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2) ->
      processExprForLiterals expr1 @ processExprForLiterals expr2
    | _ -> findInnerLiterals innerExpr

  and checkNestedLeafElements expr =
    findInnerLiterals expr
    |> List.iter (fun (isArray, innerExpr, range) ->
      checkSingleExpr isArray range innerExpr)

  let rec collectSeparatorRange = function
    | SynExpr.Sequential (expr1 = expr1; expr2 = expr2) ->
      if isLiteral expr1 && isLiteral expr2 then
        let expr1End = getLastRange expr1
        let expr2Start = getFirstRange expr2
        match expr1End, expr2Start with
        | Some endRange, Some startRange ->
          [ endRange, startRange ] @
          collectSeparatorRange expr1 @
          collectSeparatorRange expr2
        | _ -> collectSeparatorRange expr1 @ collectSeparatorRange expr2
      else
        collectSeparatorRange expr1 @ collectSeparatorRange expr2
    | SynExpr.ArrayOrListComputed (expr = innerExpr) ->
      collectSeparatorRange innerExpr
    | SynExpr.ArrayOrList (exprs = exprs) ->
      List.collect collectSeparatorRange exprs
    | _ -> []

  and isLiteral = function
    | SynExpr.ArrayOrList _ -> true
    | SynExpr.ArrayOrListComputed _ -> true
    | _ -> false

  and getLastRange = function
    | SynExpr.ArrayOrList (range = range)
    | SynExpr.ArrayOrListComputed (range = range)
    | SynExpr.Const (range = range) -> Some range
    | SynExpr.Sequential (expr2 = expr2) -> getLastRange expr2
    | _ -> None

  and getFirstRange = function
    | SynExpr.ArrayOrList (range = range)
    | SynExpr.ArrayOrListComputed (range = range)
    | SynExpr.Const (range = range) -> Some range
    | SynExpr.Sequential (expr1 = expr1) -> getFirstRange expr1
    | _ -> None

  and checkNestedLiteralSeparatorSpacing expr =
    collectSeparatorRange expr
    |> List.iter (fun (range1, range2) ->
      if range2.StartColumn - range1.EndColumn > 2 then
        raiseWithError $"Line {range1.StartLine}: Too much spacing"
      elif range2.StartColumn - range1.EndColumn = 1 then
        raiseWithError $"Line {range1.StartLine}: No spacing")

  let checkNested bindingTrivia isArray (range: range) expr =
    if range.StartLine = range.EndLine then
      checkNestedBracketBoundarySpacing isArray expr range
      checkNestedLiteralSeparatorSpacing expr
      checkNestedLeafElements expr
      checkAvoidableLineBreak bindingTrivia range
    else
      failwith $"TODO: Nested {expr}"

  let check bindingTrivia isArray (range: range) expr =
    if range.StartLine = range.EndLine then
      checkAvoidableLineBreak bindingTrivia range
      checkSingleExpr isArray range expr
    else
      failwith $"TODO: {expr}"