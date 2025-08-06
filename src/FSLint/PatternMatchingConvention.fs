module B2R2.FSLint.PatternMatchingConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private reportBarAndPatternError src range =
  reportError src range "Need a single space between bar and pattern."

let private reportBarAndMatchError src range =
  reportError src range "Bar '|' not aligned with 'match' keyword."

let private reportArrowError src range =
  reportError src range "Need a single space around arrow."

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

let private checkFuncSpacing src typarDecls (idRange: range) = function
  | SynArgPats.Pats(pats = [ SynPat.Paren(range = range) ]) ->
    let idRange =
      match (typarDecls: option<SynValTyparDecls>) with
      | Some(SynValTyparDecls(typars = Some typars)) -> typars.Range
      | _ -> idRange
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
          else
            ()
        | _ -> ()
      | SynPat.LongIdent(longDotId = SynLongIdent(id = id)
                         argPats = SynArgPats.Pats pats) ->
        match pats with
        | [ SynPat.Paren(range = range) ] ->
          if (List.last id).idRange.EndColumn + 1 <> range.StartColumn then
            reportError src range "Need single space between ident and paren"
          else
            ()
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

let rec checkRecordInPattern src (idRange: range) = function
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
      if not field.IsParen && (idRange.EndColumn + 1 <> field.Range.StartColumn)
      then reportError src field.Range "Only need a single space"
      else ()
      checkRecordFuncSpacing src field
      checkRecordOperatorSpacing src field
      checkRecordSeparatorSpacing src field
  | _ :: _ -> warn $"[RecordPattern]TODO: Various Args"
  | [] -> ()

and checkLongIdentPatternCase src typarDecls argPats = function
  | [ qualifier; method: Ident ]
    when FunctionCallConvention.isPascalCase method.idText &&
         qualifier.idText <> "_" && qualifier.idText <> "this" ->
    checkFuncSpacing src typarDecls method.idRange argPats
  | [ id ] when FunctionCallConvention.isPascalCase id.idText &&
                not argPats.Patterns.IsEmpty ->
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
    reportError src argPats.Patterns.Head.Range "Contains invalid whitespace."
  | _ -> ()

/// checks pattern cases with incorrect spacing or newlines.
let checkPatternSpacing src clauses =
  let rec check src pat outerTrivia =
    match pat with
    | SynPat.Or(lhsPat = lhsPat; rhsPat = rhsPat; trivia = trivia) ->
      check src lhsPat outerTrivia
      check src rhsPat (Some trivia.BarRange)
    | _ ->
      match outerTrivia with
      | Some range ->
        if pat.Range.StartLine <> range.StartLine then
          reportError src pat.Range "Bar is not inline with Pattern."
        elif pat.Range.StartColumn - 2 <> range.StartColumn then
          reportBarAndPatternError src range
        else
          ()
      | None ->
        ()
  clauses
  |> List.iter (fun (SynMatchClause(pat = pat; trivia = outerTrivia)) ->
    check src pat outerTrivia.BarRange
  )

let private checkArrowBySrc (src: ISourceText) arrowRange isInline =
  let checker = if isInline then " -> " else " ->"
  let line = src.GetLineString((arrowRange: range).StartLine - 1)
  if line.Contains checker then () else reportArrowError src arrowRange

/// Checks for missing or extra spaces around '->' in match cases.
let checkArrowSpacing src clauses =
  clauses
  |> List.iter (fun (SynMatchClause(resultExpr = expr; trivia = trivia)) ->
    match trivia.ArrowRange with
    | Some arrowRange ->
      checkArrowBySrc src arrowRange
        (arrowRange.StartLine = expr.Range.StartLine)
    | None -> ()
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

let checkFormat src clauses =
  checkPatternSpacing src clauses
  checkArrowSpacing src clauses

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
      pats
      |> List.unzip3 |> fun (_, _, pats) -> pats |> List.iter (checkBody src)
  | SynPat.Paren(pat = pat) ->
    checkBody src pat
  | SynPat.Tuple(elementPats = elementPats) ->
    elementPats |> List.iter (checkBody src)
  | SynPat.As(lhsPat = lhsPat; rhsPat = rhsPat)
  | SynPat.Or(lhsPat = lhsPat; rhsPat = rhsPat) ->
    checkBody src lhsPat
    checkBody src rhsPat
  | _ -> () (* no need to check this *)

and checkArrayOrList src isArray (elementPats: SynPat list) (range: range) =
  if elementPats.IsEmpty then
    let enclosureWidth = if isArray then 4 else 2
    if range.EndColumn - range.StartColumn <> enclosureWidth then
      reportError src range "Contains Invalid Whitespace"
    else
      ()
  else
    let totalRange =
      elementPats
      |> List.map (fun pat -> pat.Range)
      |> List.reduce Range.unionRanges
    ArrayOrListConvention.checkCommon src isArray range totalRange
    if totalRange.StartLine = totalRange.EndLine then
      collectElemAndOptionalSeparatorRanges src elementPats
      |> ArrayOrListConvention.checkElementSpacing src
    else
      ()
    elementPats |> List.iter (checkBody src)