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

/// True when the tuple has been spread by its own commas, some neighbour
/// having been sent to a line below the one its predecessor ends on.
///
/// An element running to several lines of its own, a record or a list, widens
/// the tuple's range without any comma having been broken. That spread is the
/// block's doing and not the tuple's, and what follows the block still sits
/// beside it; the commas are all the tuple answers for.
let private isSpread (exprs: SynExpr list) =
  exprs
  |> List.map (fun (expr: SynExpr) -> expr.Range)
  |> List.pairwise
  |> List.exists (fun (prev: range, next: range) ->
    next.StartLine > prev.EndLine)

/// True when every element keeps to a line of its own making, so that the
/// tuple could be brought onto one line at all. An element running to several
/// lines, a record or a pipeline, can never be brought up, and the tuple
/// holding it can never be one line however its commas are laid out.
let private everyElementOnOneLine (exprs: SynExpr list) =
  exprs
  |> List.forall (fun (expr: SynExpr) ->
    expr.Range.StartLine = expr.Range.EndLine)

/// The width the line would take were the neighbour brought up beside the
/// comma that follows its predecessor.
let private joinedWidth (src: ISourceText) (prev: range) (next: range) =
  let prevLine = src.GetLineString(prev.EndLine - 1)
  let nextLine = src.GetLineString(next.StartLine - 1)
  prevLine.TrimEnd().Length + 1 + (nextLine.TrimEnd().Length - next.StartColumn)

/// The first neighbour sent below the line its predecessor ends on.
let private firstBrokenGap (exprs: SynExpr list) =
  exprs
  |> List.map (fun (expr: SynExpr) -> expr.Range)
  |> List.pairwise
  |> List.tryFind (fun (prev: range, next: range) ->
    next.StartLine > prev.EndLine)

/// True when the neighbour could be brought back up beside the comma. It has
/// to fit the line, and it has to stand on a single line to begin with: a
/// block belongs at the head of a line, and pulling its opening up would
/// strand the rest of it below.
let private isLiftable src (prev: range) (next: range) =
  next.StartLine = next.EndLine
  && joinedWidth src prev next <= Diagnostics.getCurrentMaxLineLength ()

/// A tuple of data too wide for its line is not broken at its commas: a list
/// of them reads as a table, and a row spilling over its neighbours loses the
/// shape the table is read by. What it wants is a name, so that the row can
/// stand on one line again. A parameter list is the other thing entirely and
/// breaks at every comma, which is why the two are told apart first.
///
/// Only a tuple already spread by its own commas is asked this. One still
/// standing on a single line has chosen no layout to answer for: what is
/// wrong with it is the length of the line, and the line budget says so.
///
/// A tuple holding an element that runs to lines of its own can never be
/// brought onto one line, so a name is no answer to it. What is asked of that
/// one is that a neighbour come up beside the comma before it, and a name
/// wherever it cannot: a neighbour too wide for that line, or one that is
/// itself a block and so has nowhere to be brought up to.
let private checkWidth src (exprs: SynExpr list) isParameterList =
  if not isStrict || isParameterList || not (isSpread exprs) then
    false
  elif everyElementOnOneLine exprs then
    if fitsOneLine src exprs then
      false
    else
      tupleSpan exprs |> reportBindToLet src
      true
  else
    match firstBrokenGap exprs with
    | Some(prev, next) ->
      if isLiftable src prev next then reportNewLine src next
      else tupleSpan exprs |> reportBindToLet src
      true
    | None ->
      false

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