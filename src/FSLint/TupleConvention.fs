module B2R2.FSLint.TupleConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

/// True when the whole of the tuple would stand on one line inside the line
/// budget, so that spreading it is a choice rather than a necessity.
let private tupleSpan (exprs: SynExpr list) =
  exprs
  |> List.map (fun (e: SynExpr) -> e.Range)
  |> List.reduce Range.unionRanges

let private fitsOneLine src exprs =
  LineBreakConvention.closesUpWithin src (tupleSpan exprs)

/// True when the tuple has been spread over more than one line, and so has
/// made a choice of layout that it can be held to.
let private isSpread exprs =
  let span = tupleSpan exprs
  span.StartLine <> span.EndLine

/// A tuple of data too wide for its line is not broken at its commas: a list
/// of them reads as a table, and a row spilling over its neighbours loses the
/// shape the table is read by. What it wants is a name, so that the row can
/// stand on one line again. A parameter list is the other thing entirely and
/// breaks at every comma, which is why the two are told apart first.
///
/// Only a tuple already spread is asked this. One still standing on a single
/// line has chosen no layout to answer for: what is wrong with it is the
/// length of the line, and the line budget says that by itself.
let private checkWidth src (exprs: SynExpr list) isParameterList =
  if not isStrict || isParameterList then
    false
  elif not (isSpread exprs) || fitsOneLine src exprs then
    false
  else
    tupleSpan exprs |> reportBindToLet src
    true

/// Every element of a tuple must either share one line or each sit on a line
/// of its own, as the operands of any other separator list do. One short
/// enough to close up onto a single line has to be closed up first.
let checkPlacement src (exprs: SynExpr list) isParameterList =
  if checkWidth src exprs isParameterList then
    ()
  else
    exprs
    |> List.map (fun (expr: SynExpr) -> expr.Range)
    |> LineBreakConvention.checkUniformPlacement src

/// A tuple standing inside parentheses is judged along with them: the fence
/// is part of the list, and opening it sends the whole to the block form.
let checkFencedPlacement src (fence: range) exprs isParameterList =
  if checkWidth src exprs isParameterList then
    ()
  else
    exprs
    |> List.map (fun (expr: SynExpr) -> expr.Range)
    |> LineBreakConvention.checkOpenableFence src fence

let check (src: ISourceText) (exprs: SynExpr list) commaRanges =
  exprs
  |> List.pairwise
  |> List.zip commaRanges
  |> List.iter (fun (commaRange: range, (expr1, expr2)) ->
    let isConlonGap =
      if commaRange.EndColumn - commaRange.StartColumn = 2 then 1 else 0
    let beforeCommaAdjusted =
      combineRangeWithComment expr1.Range commaRange.StartRange true expr1.Range
    let afterCommaAdjusted =
      combineRangeWithComment commaRange.EndRange expr2.Range false expr2.Range
    if beforeCommaAdjusted.EndLine = commaRange.StartLine
      && beforeCommaAdjusted.EndColumn + isConlonGap <> commaRange.StartColumn
    then
      Range.mkRange "" beforeCommaAdjusted.End commaRange.Start
      |> reportCommaBeforeSpacing src
    else
      ()
    if afterCommaAdjusted.StartLine = commaRange.StartLine
      && afterCommaAdjusted.StartColumn - 1 <> commaRange.EndColumn then
      Range.mkRange "" commaRange.End afterCommaAdjusted.Start
      |> reportCommaAfterSpacing src
    else
      ()
  )

let checkPat src (pats: list<SynPat>) commaRanges =
  pats
  |> List.pairwise
  |> List.zip commaRanges
  |> List.iter (fun (commaRange: range, (pat1, pat2)) ->
    let beforeCommaAdjusted =
      combineRangeWithComment pat1.Range commaRange.StartRange true pat1.Range
    let afterCommaAdjusted =
      combineRangeWithComment commaRange.EndRange pat2.Range false pat2.Range
    if beforeCommaAdjusted.EndLine = commaRange.StartLine
      && beforeCommaAdjusted.EndColumn <> commaRange.StartColumn then
      Range.mkRange "" beforeCommaAdjusted.End commaRange.Start
      |> reportCommaBeforeSpacing src
    else
      ()
    if afterCommaAdjusted.StartLine = commaRange.StartLine
      && afterCommaAdjusted.StartColumn - 1 <> commaRange.EndColumn then
      Range.mkRange "" commaRange.End afterCommaAdjusted.Start
      |> reportCommaAfterSpacing src
    else
      ()
  )