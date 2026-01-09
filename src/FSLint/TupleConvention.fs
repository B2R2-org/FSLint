module B2R2.FSLint.TupleConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let private makeNewRangeAdjustedByGap (targetRange: range) gap =
  let reduceStart =
    Position.mkPos targetRange.StartLine (targetRange.Start.Column - gap)
  let reduceEnd =
    Position.mkPos targetRange.StartLine (targetRange.End.Column - gap)
  Range.mkRange "" reduceStart reduceEnd

let private adjustRangeByComment (src: ISourceText) rangeWithElems =
  rangeWithElems
  |> List.map (fun ((comRange: range), (fstElem: SynExpr, sndElem: SynExpr)) ->
    let betweenElementsRange =
      Range.mkRange "" fstElem.Range.End sndElem.Range.Start
    let findPointer =
      src.GetSubTextFromRange(Range.shiftStart 0 -1 sndElem.Range)
    let gapStr = src.GetSubTextFromRange betweenElementsRange
    if findPointer.Chars(0).Equals '?' then
      comRange, fstElem.Range, makeNewRangeAdjustedByGap sndElem.Range 1
    elif gapStr.IndexOf "(*" <> -1 then
      let gap = gapStr.IndexOf "*)" - gapStr.IndexOf "(*" + 3
      makeNewRangeAdjustedByGap comRange gap,
      fstElem.Range,
      makeNewRangeAdjustedByGap sndElem.Range gap
    else
      comRange, fstElem.Range, sndElem.Range
  )

let private adjustRangeByCommentInPat (src: ISourceText) rangeWithElems =
  rangeWithElems
  |> List.map (fun ((comRange: range), (fstElem: SynPat, sndElem: SynPat)) ->
    let betweenElementsRange =
      Range.mkRange "" fstElem.Range.End sndElem.Range.Start
    let findPointer =
      src.GetSubTextFromRange(Range.shiftStart 0 -1 sndElem.Range)
    let gapStr = src.GetSubTextFromRange betweenElementsRange
    if findPointer.Chars(0).Equals '?' then
      comRange, fstElem.Range, makeNewRangeAdjustedByGap sndElem.Range 1
    elif gapStr.IndexOf "(*" <> -1 then
      let gap = gapStr.IndexOf "*)" - gapStr.IndexOf "(*" + 3
      makeNewRangeAdjustedByGap comRange gap,
      fstElem.Range,
      makeNewRangeAdjustedByGap sndElem.Range gap
    else
      comRange, fstElem.Range, sndElem.Range
  )

let filterOutConsOperators exprs =
  exprs
  |> List.filter (function
    | SynExpr.App(isInfix = true
                  funcExpr = SynExpr.LongIdent(
                    longDotId = SynLongIdent(
                      id = [ id ]))) when id.idText = "op_ColonColon" ->
      false
    | _ -> true)

let check (src: ISourceText) exprs commaRanges =
  exprs
  |> List.pairwise
  |> List.zip commaRanges
  |> adjustRangeByComment src
  |> List.iter (fun (commaRange, fstElemRange, sndElemRange) ->
    let gapStr =
      Range.unionRanges fstElemRange commaRange
      |> src.GetSubTextFromRange
    let symb = if gapStr.TrimEnd().EndsWith("::") then "'::'" else "','"
    let gap = commaRange.EndColumn - commaRange.StartColumn - 1
    if fstElemRange.EndColumn + gap <> commaRange.StartColumn then
      Range.mkRange "" fstElemRange.End commaRange.Start
      |> fun range ->
        if symb = "'::'" then
          reportWarn src range $"Use single whitespace before {symb}"
        else
          reportWarn src range "Remove whitespace before ','"
    elif sndElemRange.StartColumn - 1 <> commaRange.EndColumn
      && sndElemRange.StartLine = commaRange.StartLine then
      Range.mkRange "" commaRange.End sndElemRange.Start
      |> fun range -> reportWarn src range $"Use single whitespace after {symb}"
    else
      ()
  )

let checkPat src pats commaRanges =
  pats
  |> List.pairwise
  |> List.zip commaRanges
  |> adjustRangeByCommentInPat src
  |> List.iter (fun (commaRange, fstElemRange, sndElemRange) ->
    let gap = commaRange.EndColumn - commaRange.StartColumn - 1
    if fstElemRange.EndColumn + gap <> commaRange.StartColumn then
      Range.mkRange "" fstElemRange.End commaRange.Start
      |> fun range -> reportWarn src range "Remove whitespace before ','"
    elif sndElemRange.StartColumn - 1 <> commaRange.EndColumn
      && sndElemRange.StartLine = commaRange.StartLine then
      Range.mkRange "" commaRange.End sndElemRange.Start
      |> fun range -> reportWarn src range "Use single whitespace after ','"
    else
      ()
  )