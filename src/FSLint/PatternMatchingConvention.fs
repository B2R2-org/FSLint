module B2R2.FSLint.PatternMatchingConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let rec private collectRecordEdgeRange acc = function
  | SynPat.Named(range = range)
  | SynPat.LongIdent(range = range) ->
    match acc with
    | (idRange, _, recordRange) :: _ -> idRange, Some range, Some recordRange
    | [] -> None, None, None
  | SynPat.Record(fieldPats, recordRange) ->
    let (_, id), _, _ = List.head fieldPats
    let _, _, pat = List.last fieldPats
    collectRecordEdgeRange ((Some id.idRange, None, recordRange) :: acc) pat
  | _ -> None, None, None

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

/// Checks if the given pattern contains record with incorrect bracket spacing,
/// such as `{field}` instead of `{ field }`, within the specified range.
let checkRecordBracketSpacing src (range: range) (innerRange: range) =
  if range.StartColumn + 2 <> innerRange.StartColumn
    || range.EndColumn - 2 <> innerRange.EndColumn
  then reportError src range "Wrong spacing inside brackets"
  else ()

/// Checks for incorrect spacing in record pattern matching.
let checkRecordFuncSpacing src = function
  | SynPat.Record(fieldPats, _) ->
    fieldPats
    |> List.iter (fun (_, _, pat) ->
      match pat with
      | SynPat.LongIdent(longDotId = SynLongIdent(id = id)
                         argPats = SynArgPats.Pats pats)
        when FunctionCallConvention.isPascalCase (List.last id).idText ->
        match pats with
        | [ SynPat.Paren(range = range) ] ->
          if (List.last id).idRange.EndColumn <> range.StartColumn then
            reportError src range "No space between ident and paren"
          else ()
        | _ -> ()
      | SynPat.LongIdent(longDotId = SynLongIdent(id = id)
                         argPats = SynArgPats.Pats pats) ->
        match pats with
        | [ SynPat.Paren(range = range) ] ->
          if (List.last id).idRange.EndColumn + 1 <> range.StartColumn then
            reportError src range "Need single space between ident and paren"
          else ()
        | _ -> ()
      | _ -> ()
    )
  | _ -> ()

/// Checks spacing around the '=' operator within record definitions.
/// Ensures that the '=' operator inside records has proper spacing.
let checkRecordOperatorSpacing (src: ISourceText) = function
  | SynPat.Record(fieldPats, _) ->
    fieldPats
    |> List.iter (fun ((_, id), oprRange, pat) ->
      if oprRange.IsSome then
        if id.idRange.EndColumn + 1 <> oprRange.Value.StartColumn
          || oprRange.Value.EndColumn + 1 <> pat.Range.StartColumn
        then reportError src id.idRange "" "Need single space around '='"
        else ()
      else ()
    )
  | _ -> ()

/// Checks whether semicolons are properly placed within a record pattern.
let checkRecordSeparatorSpacing (src: ISourceText) (field: SynPat) =
  if field.Range.StartLine <> field.Range.EndLine then
    src.GetSubTextFromRange field.Range
    |> fun subStr ->
      if subStr.Contains ';' then
        reportError src field.Range "Contains Invalid Separator"
      else ()
  else
    match field with
    | SynPat.Record(fieldPats, _) when fieldPats.Length > 1 ->
      field.Range
      |> src.GetSubTextFromRange
      |> fun subStr ->
        subStr
        |> Seq.mapi (fun i c -> if c = ';' then Some i else None)
        |> Seq.choose id
        |> Seq.iter (fun index ->
          if index > 0 && subStr[index - 1] = ' ' then
            reportError src field.Range "No space before semicolon"
          elif index < subStr.Length - 1 then
            match subStr[index + 1] with
            | ' ' when index < subStr.Length - 2 && subStr.[index + 2] = ' ' ->
              reportError src field.Range "Need single space around semicolon"
            | ' ' -> ()
            | _ -> reportError src field.Range "Missing space after semicolon"
          else ()
        )
    | _ -> ()

let rec checkRecordPattern src (idRange: range) = function
  | [ field: SynPat ] ->
    match field with
    | SynPat.Paren(pat = pat) ->
      if pat.IsRecord then
        checkRecordPattern src idRange [ pat ]
      else ()
    | _ ->
      match collectRecordEdgeRange [] field with
      | Some startRange, Some endRange, Some range ->
        Range.unionRanges startRange endRange
        |> checkRecordBracketSpacing src range
      | _ -> ()
      if not field.IsParen && (idRange.EndColumn + 1
        <> field.Range.StartColumn)
      then reportError src field.Range "Only need a single space"
      else ()
      checkRecordFuncSpacing src field
      checkRecordOperatorSpacing src field
      checkRecordSeparatorSpacing src field
  | _ :: _ -> warn $"[RecordPattern]TODO: Various Args"
  | [] -> ()

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
    | [ id ]
      when FunctionCallConvention.isPascalCase id.idText
        && not argPats.Patterns.IsEmpty
        && argPats.Patterns.Head.IsRecord ->
      checkRecordPattern src id.idRange argPats.Patterns
    | [ id ]
      when FunctionCallConvention.isPascalCase id.idText
      && not argPats.Patterns.IsEmpty
      && argPats.Patterns.Head.IsParen ->
      match argPats.Patterns.Head with
      | SynPat.Record _ -> checkRecordPattern src id.idRange argPats.Patterns
      | _ -> checkFuncSpacing src id.idRange argPats
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