module B2R2.FSLint.TupleConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

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