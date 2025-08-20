module B2R2.FSLint.TupleConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let makeNewRangeAdjustedByGap (targetRange: range) gap =
  let reduceStart =
    Position.mkPos targetRange.StartLine (targetRange.Start.Column - gap)
  let reduceEnd =
    Position.mkPos targetRange.StartLine (targetRange.End.Column - gap)
  Range.mkRange "" reduceStart reduceEnd

let adjustRangeByComment (src: ISourceText) rangeWithElems =
  rangeWithElems
  |> List.map (fun ((comRange: range), (fstElem: SynExpr, sndElem: SynExpr)) ->
    let betweenElementsRange =
      Range.mkRange "" fstElem.Range.End sndElem.Range.Start
    let findPointer =
      src.GetSubTextFromRange(Range.shiftStart 0 -1 sndElem.Range)
    let gapStr = src.GetSubTextFromRange betweenElementsRange
    if findPointer.Chars(0).Equals '?' then
      comRange, fstElem.Range, makeNewRangeAdjustedByGap sndElem.Range 1
    elif gapStr.IndexOf "(*" <> - 1 then
      let gap = gapStr.IndexOf "*)" - gapStr.IndexOf "(*" + 3
      makeNewRangeAdjustedByGap comRange gap,
      fstElem.Range,
      makeNewRangeAdjustedByGap sndElem.Range gap
    else comRange, fstElem.Range, sndElem.Range
  )

let adjustRangeByCommentInPat (src: ISourceText) rangeWithElems =
  rangeWithElems
  |> List.map (fun ((comRange: range), (fstElem: SynPat, sndElem: SynPat)) ->
    let betweenElementsRange =
      Range.mkRange "" fstElem.Range.End sndElem.Range.Start
    let findPointer =
      src.GetSubTextFromRange(Range.shiftStart 0 -1 sndElem.Range)
    let gapStr = src.GetSubTextFromRange betweenElementsRange
    if findPointer.Chars(0).Equals '?' then
      comRange, fstElem.Range, makeNewRangeAdjustedByGap sndElem.Range 1
    elif gapStr.IndexOf "(*" <> - 1 then
      let gap = gapStr.IndexOf "*)" - gapStr.IndexOf "(*" + 3
      makeNewRangeAdjustedByGap comRange gap,
      fstElem.Range,
      makeNewRangeAdjustedByGap sndElem.Range gap
    else comRange, fstElem.Range, sndElem.Range
  )

let check src exprs commaRanges =
  exprs
  |> List.pairwise
  |> List.zip commaRanges
  |> adjustRangeByComment src
  |> List.iter (fun (commaRange, fstElemRange, sndElemRange) ->
    let gap = commaRange.EndColumn - commaRange.StartColumn - 1
    if fstElemRange.EndColumn + gap <> commaRange.StartColumn then
      reportError src commaRange "No space allowed before comma"
    elif sndElemRange.StartColumn - 1 <> commaRange.EndColumn
      && sndElemRange.StartLine = commaRange.StartLine then
      reportError src commaRange "Need to space after comma"
    else ()
  )

let checkPat src pats commaRanges =
  pats
  |> List.pairwise
  |> List.zip commaRanges
  |> adjustRangeByCommentInPat src
  |> List.iter (fun (commaRange, fstElemRange, sndElemRange) ->
    let gap = commaRange.EndColumn - commaRange.StartColumn - 1
    if fstElemRange.EndColumn + gap <> commaRange.StartColumn then
      reportError src commaRange "No space allowed before comma"
    elif sndElemRange.StartColumn - 1 <> commaRange.EndColumn
      && sndElemRange.StartLine = commaRange.StartLine then
      reportError src commaRange "Need to space after comma"
    else ()
  )