module B2R2.FSLint.PatternMatchingConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let rec private collectRecordEdgeRange acc = function
  | SynPat.Named(range = range)
  | SynPat.LongIdent(range = range) ->
    match acc with
    | (idRange, _, recordRange) :: _ -> idRange, Some range, Some recordRange
    | [] -> None, None, None
  | SynPat.Record(fieldPats, recordRange) ->
    let id = (List.head fieldPats).FieldName.LongIdent
    let pat = (List.last fieldPats).Pattern
    collectRecordEdgeRange ((Some id.Head.idRange, None, recordRange) :: acc)
      pat
  | _ ->
    None, None, None

let private checkFuncSpacing src typarDecls (idRange: range) = function
  | SynArgPats.Pats(pats = [ SynPat.Paren(range = range) ]) ->
    let idRange =
      match (typarDecls: option<SynValTyparDecls>) with
      | Some(SynValTyparDecls(typars = Some typars)) -> typars.Range
      | _ -> idRange
    if idRange.EndColumn <> range.StartColumn then
      reportPascalCaseError src <| Range.mkRange "" idRange.End range.Start
    else
      ()
  | SynArgPats.NamePatPairs(trivia = trivia) ->
    if idRange.EndColumn <> trivia.ParenRange.StartColumn then
      Range.mkRange "" idRange.End trivia.ParenRange.Start
      |> reportPascalCaseError src
    else
      ()
  | _ ->
    ()

let private collectElemAndOptSeparatorRanges (src: ISourceText) elementPats =
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
    | _ ->
      warn $"[PatternMatching] Pattern ParsingFailure"
      []
  interleave elementRanges separatorRanges []

/// Checks cons operator in the context is properly surrounded by single spaces.
let private checkConsOperatorSpacing src lhsRange rhsRange (colonRange: range) =
  let beforeColonAdjusted =
    combineRangeWithComment lhsRange colonRange.StartRange true lhsRange
  let afterColonAdjusted =
    combineRangeWithComment colonRange.EndRange (rhsRange: range).StartRange
      false rhsRange
  if beforeColonAdjusted.EndColumn + 1 <> colonRange.StartColumn
    && beforeColonAdjusted.StartLine = colonRange.StartLine
  then
    Range.mkRange "" beforeColonAdjusted.End colonRange.Start
    |> fun range -> reportWarn src range "Use single whitespace before ':'"
  elif afterColonAdjusted.StartColumn - 1 <> colonRange.EndColumn
    && afterColonAdjusted.StartLine = colonRange.StartLine
  then
    Range.mkRange "" colonRange.End afterColonAdjusted.Start
    |> fun range -> reportWarn src range "Use single whitespace after ':'"
  else ()

/// Checks if the given pattern contains record with incorrect bracket spacing,
/// such as `{field}` instead of `{ field }`, within the specified range.
let private checkRecordBracketSpacing src (range: range) (innerRange: range) =
  if range.StartColumn + 2 <> innerRange.StartColumn then
    Range.mkRange "" range.Start innerRange.Start
    |> reportLeftCurlyBraceSpacing src
  elif range.EndColumn - 2 <> innerRange.EndColumn then
    Range.mkRange "" innerRange.End range.End
    |> reportRightCurlyBraceSpacing src
  else ()

/// Checks for incorrect spacing in record pattern matching.
let private checkRecordFuncSpacing src = function
  | SynPat.Record(fieldPats, _) ->
    fieldPats
    |> List.iter (function
      | NamePatPairField(pat = pat) ->
        match pat with
        | SynPat.LongIdent(longDotId = SynLongIdent(id = id)
                           argPats = SynArgPats.Pats pats)
          when isPascalCase (List.last id).idText ->
          match pats with
          | [ SynPat.Paren(range = range) ] ->
            if (List.last id).idRange.EndColumn <> range.StartColumn then
              Range.mkRange "" (List.last id).idRange.End range.Start
              |> reportPascalCaseError src
            else
              ()
          | _ ->
            ()
        | SynPat.LongIdent(longDotId = SynLongIdent(id = id)
                           argPats = SynArgPats.Pats pats) ->
          match pats with
          | [ SynPat.Paren(range = range) ] ->
            if (List.last id).idRange.EndColumn + 1 <> range.StartColumn then
              Range.mkRange "" (List.last id).idRange.End range.Start
              |> reportLowerCaseError src
            else
              ()
          | _ ->
            ()
        | _ ->
          ()
    )
  | _ ->
    ()

/// Checks spacing around the '=' operator within record definitions.
/// Ensures that the '=' operator inside records has proper spacing.
let private checkRecordOperatorSpacing (src: ISourceText) = function
  | SynPat.Record(fields, _) ->
    fields
    |> List.iter (function
      | NamePatPairField(fieldName = ident; equalsRange = symbolRange
                         pat = pat) ->
      if symbolRange.IsSome then
        if ident.Range.EndColumn + 1 <> symbolRange.Value.StartColumn
          || symbolRange.Value.EndColumn + 1 <> pat.Range.StartColumn
        then
          Range.mkRange "" ident.Range.End pat.Range.Start
          |> reportInfixSpacing src
        else
          ()
      else
        ()
    )
  | _ ->
    ()

/// Checks whether semicolons are properly placed within a record pattern.
let private checkRecordSeparatorSpacing (src: ISourceText) (field: SynPat) =
  if field.Range.StartLine <> field.Range.EndLine then
    src.GetSubTextFromRange field.Range
    |> fun subStr ->
      if subStr.Contains ';' then reportTrailingSeparator src field.Range
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
            reportSemiColonBeforeSpacing src field.Range
          elif index < subStr.Length - 1 then
            match subStr[index + 1] with
            | ' ' when index < subStr.Length - 2 && subStr.[index + 2] = ' ' ->
              reportSemiColonAfterSpacing src field.Range
            | ' ' -> ()
            | _ ->
              reportSemiColonAfterSpacing src field.Range
          else
            ()
        )
    | _ ->
      ()

let rec private checkRecordInPattern src (idRange: range) = function
  | [ field: SynPat ] ->
    match field with
    | SynPat.Paren(pat = pat) ->
      if pat.IsRecord then checkRecordInPattern src idRange [ pat ]
      else ()
    | _ ->
      match collectRecordEdgeRange [] field with
      | Some startRange, Some endRange, Some range ->
        Range.unionRanges startRange endRange
        |> checkRecordBracketSpacing src range
      | _ -> ()
      if not field.IsParen && idRange.EndColumn + 1 <> field.Range.StartColumn
      then reportWarn src field.Range "Use single whitespace"
      else ()
      checkRecordFuncSpacing src field
      checkRecordOperatorSpacing src field
      checkRecordSeparatorSpacing src field
  | _ :: _ -> warn $"[RecordPattern]TODO: Various Args"
  | [] -> ()

and private checkLongIdentPatternCase src typarDecls argPats = function
  | [ qualifier; method: Ident ]
    when isPascalCase method.idText &&
         qualifier.idText <> "_" && qualifier.idText <> "this" ->
    checkFuncSpacing src typarDecls method.idRange argPats
  | [ id ] when isPascalCase id.idText && not argPats.Patterns.IsEmpty ->
    if argPats.Patterns.Head.IsRecord then
      checkRecordInPattern src id.idRange argPats.Patterns
    elif argPats.Patterns.Head.IsParen then
      match argPats.Patterns.Head with
      | SynPat.Record _ -> checkRecordInPattern src id.idRange argPats.Patterns
      | _ -> checkFuncSpacing src typarDecls id.idRange argPats
    else
      ()
  | [ id ]
    when id.idText = "new" && argPats.Patterns.Head.IsParen &&
         id.idRange.EndColumn <> argPats.Patterns.Head.Range.StartColumn ->
    Range.mkRange "" id.idRange.End argPats.Patterns.Head.Range.Start
    |> reportPascalCaseError src
  | _ -> ()

/// checks pattern cases with incorrect spacing or newlines.
let private checkPatternSpacing src clauses =
  let rec check src pat outerTrivia =
    match pat with
    | SynPat.Or(lhsPat = lhsPat; rhsPat = rhsPat; trivia = trivia) ->
      check src lhsPat outerTrivia
      check src rhsPat (Some trivia.BarRange)
    | _ ->
      match outerTrivia with
      | Some range ->
        if pat.Range.StartLine <> range.StartLine then
          reportWarn src pat.Range "Move '|' inline with pattern"
        elif pat.Range.StartColumn - 2 <> range.StartColumn then
          Range.mkRange "" range.Start pat.Range.Start
          |> reportBarAfterSpacing src
        else
          ()
      | None ->
        ()
  clauses
  |> List.iter (fun (SynMatchClause(pat = pat; trivia = outerTrivia)) ->
    check src pat outerTrivia.BarRange
  )

/// Checks for missing or extra spaces around '->' in match cases.
let checkArrowSpacing src patRange whenExpr (bodyRange: range)
  (arrowRange: range) =
  let patRange =
    if Option.isSome (whenExpr: option<SynExpr>) then whenExpr.Value.Range
    else patRange
  let patRangeAdjusted =
    combineRangeWithComment patRange arrowRange.StartRange true patRange
  let bodyRangeAdjusted =
    combineRangeWithComment arrowRange.EndRange bodyRange.StartRange false
      bodyRange
  if (patRange: range).EndLine = (bodyRange: range).StartLine then
    if patRangeAdjusted.EndColumn + 1 <> arrowRange.StartColumn then
        Range.mkRange "" patRangeAdjusted.End arrowRange.Start
        |> reportArrowBeforeSpacing src
    elif arrowRange.EndColumn + 1 <> bodyRangeAdjusted.StartColumn then
      Range.mkRange "" arrowRange.End bodyRangeAdjusted.Start
      |> fun range -> reportArrowAfterSpacing src range
    elif patRangeAdjusted.EndColumn = arrowRange.StartColumn
      && arrowRange.EndColumn = bodyRangeAdjusted.StartColumn then
      Range.mkRange "" patRangeAdjusted.End bodyRangeAdjusted.Start
      |> fun range -> reportWarn src range "Use single whitespace around '->'"
    else
      ()
  else
    if patRange.EndLine = arrowRange.StartLine then
      if patRangeAdjusted.EndColumn + 1 <> arrowRange.StartColumn then
        Range.mkRange "" patRangeAdjusted.End arrowRange.Start
        |> reportArrowBeforeSpacing src
      else
        ()
    elif arrowRange.EndColumn + 1 <> bodyRangeAdjusted.StartColumn then
      Range.mkRange "" arrowRange.End bodyRangeAdjusted.Start
      |> reportArrowAfterSpacing src
    else
      ()

let checkParenTupleSpacing src (pats: SynPat list) =
  pats
  |> List.iter (fun pat ->
    match pat with
    | SynPat.Paren(pat = innerPat) ->
      match innerPat with
      | SynPat.Tuple(elementPats = elementPats; commaRanges = commaRanges) ->
        TupleConvention.checkPat src elementPats commaRanges
      | _ ->
        ()
      ParenConvention.checkPat src pat
    | _ ->
      ()
  )

/// Checks that '|' is vertically aligned with its 'match' keyword.
let checkBarIsSameColWithMatch src clauses (trivia: SynExprMatchTrivia) =
  let rec collectBarsFromPattern currentPat acc =
    match currentPat with
    | SynPat.Or(lhsPat = lhs; rhsPat = rhs; trivia = innerTrivia) ->
      collectBarsFromPattern rhs
        (collectBarsFromPattern lhs (innerTrivia.BarRange :: acc))
    | _ ->
      acc
  clauses
  |> List.iter (fun (SynMatchClause(pat = pat; trivia = outerTrivia)) ->
    match outerTrivia.BarRange with
    | Some barRange -> barRange :: collectBarsFromPattern pat []
    | _ -> collectBarsFromPattern pat []
    |> List.groupBy (fun range -> range.StartLine)
    |> List.map (fun (line, ranges) ->
      ranges |> List.minBy (fun range -> range.StartColumn)
    )
    |> List.iter (fun barRange ->
      if trivia.MatchKeyword.StartColumn <> barRange.StartColumn then
        reportBarAndMatchError src barRange
      else
        ()
    )
  )

let checkFormat src clauses = checkPatternSpacing src clauses

let rec checkBody (src: ISourceText) = function
  | SynPat.ArrayOrList(isArray, elementPats, range) ->
    checkArrayOrList src isArray elementPats range
  | SynPat.ListCons(lhsPat = lhsPat; rhsPat = rhsPat; trivia = triv) ->
    checkConsOperatorSpacing src lhsPat.Range rhsPat.Range triv.ColonColonRange
    checkBody src lhsPat
    checkBody src rhsPat
  | SynPat.LongIdent(longDotId = SynLongIdent(id = id)
                     typarDecls = typarDecls
                     argPats = argPats) ->
    checkLongIdentPatternCase src typarDecls argPats id
    match argPats with
    | SynArgPats.Pats(pats = pats) ->
      pats |> List.iter (checkBody src)
    | SynArgPats.NamePatPairs(pats = pats) ->
      pats |> List.iter (fun pat -> checkBody src pat.Pattern)
  | SynPat.Paren(pat = pat) ->
    checkBody src pat
  | SynPat.Tuple(elementPats = elementPats; commaRanges = commaRanges) ->
    TupleConvention.checkPat src elementPats commaRanges
    elementPats |> List.iter (checkBody src)
  | SynPat.As(lhsPat = lhsPat; rhsPat = rhsPat)
  | SynPat.Or(lhsPat = lhsPat; rhsPat = rhsPat) ->
    checkBody src lhsPat
    checkBody src rhsPat
  | _ -> () (* no need to check this *)

and private checkArrayOrList src isArray elementPats (range: range) =
  if elementPats.IsEmpty then
    let enclosureWidth = if isArray then 4 else 2
    if range.EndColumn - range.StartColumn <> enclosureWidth then
      Range.mkRange "" range.Start range.End |> reportBracketNoSpacingError src
    else
      ()
  else
    let elemRanges =
      elementPats
      |> List.map (fun pat -> pat.Range)
      |> List.reduce Range.unionRanges
    ArrayOrListConvention.checkCommon src isArray range elemRanges
    if elemRanges.StartLine = elemRanges.EndLine then
      collectElemAndOptSeparatorRanges src elementPats
      |> ArrayOrListConvention.checkElementSpacing src
    else
      ()
    elementPats |> List.iter (checkBody src)