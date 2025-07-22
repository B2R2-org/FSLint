module B2R2.FSLint.PatternMatchingConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private checkFuncSpacing src (idRange: range) = function
  | SynArgPats.Pats(pats = [ SynPat.Paren(range = range) ]) ->
    if idRange.EndColumn <> range.StartColumn then
      reportError src range "Contains invalid whitespace"
    else ()
  | SynArgPats.NamePatPairs(trivia = trivia) ->
    if idRange.EndColumn <> trivia.ParenRange.StartColumn then
      reportError src trivia.ParenRange "Contains invalid whitespace"
    else ()
  | _ -> ()

let collectElemAndOptionalSeparatorRanges (src: ISourceText) elementPats =
  let separatorRanges =
    (elementPats: SynPat list)
    |> List.map (fun pat -> pat.Range)
    |> List.pairwise
    |> List.map (fun (fstRange, sndRange) ->
      let betweenElementsRange = Range.mkRange "" fstRange.End sndRange.Start
      let betweenElementsText = src.GetSubTextFromRange betweenElementsRange
      let semicolonIndex = betweenElementsText.IndexOf ";" + fstRange.EndColumn
      let startPos = Position.mkPos fstRange.StartLine semicolonIndex
      let endPos = Position.mkPos fstRange.StartLine (semicolonIndex + 1)
      Range.mkRange "" startPos endPos)
  let elementRanges = List.map (fun (pat: SynPat) -> pat.Range) elementPats
  let rec interleave elements separators acc =
    match elements, separators with
    | [], [] -> List.rev acc
    | [ elem ], [] -> List.rev (elem :: acc)
    | elem :: restElems, sep :: restSeps ->
      interleave restElems restSeps (sep :: elem :: acc)
    | _ -> reportError src elementPats.Head.Range "Pattern ParsingFailure"
  interleave elementRanges separatorRanges []

/// Checks cons operator in the context is properly surrounded by single spaces.
let checkConsOperatorSpacing src lhsRange rhsRange (colonRange: range) =
  if (lhsRange: range).EndColumn + 1 <> colonRange.StartColumn
    || (rhsRange: range).StartColumn - 1 <> colonRange.EndColumn then
    reportError src colonRange "Cons must be surrounded by single spaces"
  else ()

let rec check (src: ISourceText) = function
  | SynPat.ArrayOrList(isArray, elementPats, range) ->
    if elementPats.IsEmpty then
      let enclosureWidth = if isArray then 4 else 2
      if range.EndColumn - range.StartColumn <> enclosureWidth then
        reportError src range "Contains Invalid Whitespace"
      else ()
    else
      elementPats
      |> List.map (fun pat -> pat.Range)
      |> List.reduce Range.unionRanges
      |> ArrayOrListConvention.checkCommon src isArray range
      collectElemAndOptionalSeparatorRanges src elementPats
      |> ArrayOrListConvention.checkElementSpacing src
      elementPats |> List.iter (check src)
  | SynPat.ListCons(lhsPat = lhsPat; rhsPat = rhsPat; trivia = triv) ->
    checkConsOperatorSpacing src lhsPat.Range rhsPat.Range triv.ColonColonRange
    check src lhsPat
    check src rhsPat
  | SynPat.LongIdent(longDotId = SynLongIdent(id = id); argPats = argPats) ->
    match id with
    | [ qualifier; method ]
      when FunctionCallConvention.isPascalCase method.idText
        && qualifier.idText <> "_" && qualifier.idText <> "this" ->
      checkFuncSpacing src method.idRange argPats
    | [ id ] when FunctionCallConvention.isPascalCase id.idText ->
      checkFuncSpacing src id.idRange argPats
    | _ -> ()
    match argPats with
    | SynArgPats.Pats(pats = pats) ->
      pats |> List.iter (check src)
    | SynArgPats.NamePatPairs(pats = pats) ->
      pats |> List.unzip3 |> fun (_, _, pats) -> pats |> List.iter (check src)
  | SynPat.Paren(pat = pat) ->
    check src pat
  | SynPat.Tuple(elementPats = elementPats) ->
    elementPats |> List.iter (check src)
  | SynPat.Or(lhsPat = lhsPat; rhsPat = rhsPat) ->
    check src lhsPat
    check src rhsPat
  | _ -> () (* no need to check this *)