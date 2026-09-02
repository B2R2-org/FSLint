module B2R2.FSLint.RecordConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let private getFieldRange (SynExprRecordField(fieldName = fieldName)) =
  match fst fieldName with
  | SynLongIdent(id = id) -> Some id.Head.idRange

let private getFieldLastRange (SynExprRecordField(fieldName = fieldName)) =
  match fst fieldName with
  | SynLongIdent(id = id) -> Some <| (List.last id).idRange

let private getExprRange (SynExprRecordField(expr = expr)) =
  expr |> Option.map (fun e -> e.Range)

/// Checks if the given pattern contains record with incorrect bracket spacing,
/// such as `{field}` instead of `{ field }`, within the specified range.
let private checkBracketSpacing src (range: range) (innerRange: range) =
  if range.StartColumn + 2 <> innerRange.StartColumn then
    Range.mkRange "" range.Start innerRange.Start
    |> reportLeftCurlyBraceSpacing src
  elif range.EndColumn - 2 <> innerRange.EndColumn then
    Range.mkRange "" innerRange.End range.End
    |> reportRightCurlyBraceSpacing src
  else
    ()

/// Checks for correct spacing around ':' in record field definitions.
/// Ensures the format `Field: type`.
///
/// What is read is the gap between the name and the type, so both ends are
/// taken from where those two stand rather than from the row the field happens
/// to end on: a type running onto a second row leaves the field ending well
/// below its own name, and reading the gap on that row reads some other part of
/// the field altogether.
///
/// A type opening on a row of its own has no gap here to answer for. Where it
/// went is a question of line breaks, and it is asked elsewhere.
let private checkFieldTypeSpacing (src: ISourceText) fields =
  List.iter (fun field ->
    let SynField(idOpt = idOpt; fieldType = fieldType) = field
    match idOpt with
    | None ->
      ()
    | Some id ->
      let nameEnd = id.idRange.End
      let typeStart = fieldType.Range.Start
      if nameEnd.Line <> typeStart.Line then
        ()
      else
        Range.mkRange "" nameEnd typeStart
        |> fun colonRange ->
          if src.GetSubTextFromRange colonRange <> ": " then
            reportWarn src colonRange "Use ': ' between field and type"
          else
            ()
  ) fields

/// Where the brace opening a record definition stands.
///
/// The range the parser gives for the definition opens at the access modifier
/// when the definition carries one, so the brace is looked for rather than
/// taken to be where the definition begins. Reading the modifier as the brace
/// puts every column below it two columns out.
let private openingBrace (src: ISourceText) (range: range) =
  let mutable line = range.StartLine
  let mutable found = None
  while found.IsNone && line <= range.EndLine do
    let text = src.GetLineString(line - 1)
    let start = if line = range.StartLine then range.StartColumn else 0
    let index = if start < text.Length then text.IndexOf('{', start) else -1
    if index >= 0 then found <- Some(Position.mkPos line index)
    else line <- line + 1
  found

/// The brace closing a record definition, so that a report can point at it
/// rather than at the field above it.
let private closingBrace (range: range) =
  Range.mkRange ""
    (Position.mkPos range.EndLine (max 0 (range.EndColumn - 1)))
    range.End

/// A record definition writes its braces one of two ways: each beside the
/// field it fences, or each on a line of its own. Which of the two is left to
/// whoever writes it -- one is the tighter, the other leaves the fields a
/// column of their own -- but they are read as a pair, and taking the top from
/// one and the bottom from the other is not a third way. It reads as a line
/// gone missing.
///
/// A brace beside its field takes one space between the two. A brace on a line
/// of its own has no such gap to answer for, so the spacing is asked only of
/// the layout that has one.
/// True when a brace has anything beside it on the side of its row given.
///
/// Read off the row rather than from where the fields are, so that a comment
/// keeping a brace company counts as company. A comment is prose and its place
/// is the author's business, and a rule on where the braces went must not turn
/// on it: with the fields alone standing for company, a note left in front of a
/// closing brace made a record that agrees with itself look as though it did
/// not.
let private hasCompany (text: string) = text.Trim().Length > 0

/// Which of the two layouts a record definition is written in, or none.
///
/// A definition either keeps its fields beside its braces, or opens on a brace
/// left at the end of the row the definition itself opens on and closes on a
/// brace of its own. A brace standing alone on a row below the `=` is neither:
/// nothing shares its row, so it reads as a row that lost whatever belonged on
/// it.
type private BraceLayout =
  /// `{ A: int` ... `B: int }`
  | Beside
  /// `type T = {` ... `}`
  | Fenced
  | Neither

let private checkFieldIsInlineWithBracket src (fullRange: range) fields =
  if List.isEmpty fields then
    ()
  else
    let innerRange =
      fields
      |> List.map (fun (SynField(range = range)) -> range)
      |> List.reduce Range.unionRanges
    match openingBrace src fullRange with
    | None ->
      ()
    | Some brace ->
      let openRow = (src: ISourceText).GetLineString(brace.Line - 1)
      let closeRow = src.GetLineString(fullRange.EndLine - 1)
      let afterBrace = openRow.Substring(min (brace.Column + 1) openRow.Length)
      let beforeBrace =
        let upTo = max 0 (fullRange.EndColumn - 1)
        closeRow.Substring(0, min upTo closeRow.Length)
      let beforeOpen = openRow.Substring(0, min brace.Column openRow.Length)
      let openIsBeside = hasCompany afterBrace
      let closeIsBeside = hasCompany beforeBrace
      (* What the opening brace has beside it says which layout was chosen, and
         what the closing one has beside it has to answer the same way. *)
      let layout =
        if openIsBeside then Beside
        elif hasCompany beforeOpen then Fenced
        else Neither
      let agrees =
        match layout with
        | Beside -> closeIsBeside
        | Fenced -> not closeIsBeside
        | Neither -> false
      (* The spacing is a matter of columns, so it is asked only where the
         brace and the field it fences actually share a row. *)
      let openSharesRow = brace.Line = innerRange.StartLine
      let closeSharesRow = fullRange.EndLine = innerRange.EndLine
      let innerEnd = innerRange.EndColumn
      if not agrees then
        if isStrict then
          closingBrace fullRange |> reportBracketSymmetry src
        else
          ()
      elif openSharesRow && brace.Column + 2 <> innerRange.StartColumn then
        Range.mkRange "" brace innerRange.Start
        |> reportLeftCurlyBraceSpacing src
      elif closeSharesRow && fullRange.EndColumn - 2 <> innerEnd then
        Range.mkRange "" innerRange.End fullRange.End
        |> reportRightCurlyBraceSpacing src
      else
        ()

let private checkBracketCompFlag src fullRange fieldRange exprRange =
  if isStrict then
    if (fullRange: range).StartLine <> (fieldRange: range).StartLine then
      reportWarn src fieldRange "Move '{' to inline with '='"
    elif (exprRange: range).EndLine <> fullRange.EndLine then
      (Position.mkPos (exprRange.EndLine + 1) 0,
       Position.mkPos fullRange.EndLine 0)
      ||> Range.mkRange ""
      |> src.GetSubTextFromRange
      |> fun subStr ->
        let strArr = subStr.Split([| '\n' |], StringSplitOptions.None)
        let flagStartWrong =
          (Array.head strArr).TrimStart().StartsWith "#if" |> not
        let flagEndWrong = Array.last strArr |> String.IsNullOrEmpty |> not
        if flagStartWrong && flagEndWrong then
          reportWarn src fullRange "Move field to inline with Bracket"
        else
          ()
    else
      ()
  else
    ()

/// Checks for correct spacing and formatting in record field assignments,
/// ensuring the format `{ field = expr }` is used instead of `{field = expr}`.
/// Also validates bracket positioning and formatting in multi-line records.
/// Where the braces are read from: the first field, or the expression being
/// copied from where the record was written with `with`.
let private innerEdges copyInfo fields =
  match getFieldRange (List.head fields), getExprRange (List.last fields) with
  | Some fieldRange, Some exprRange ->
    let fieldRange =
      match (copyInfo: option<SynExpr * _>) with
      | Some(expr, _) -> expr.Range
      | _ -> fieldRange
    Some(fieldRange, exprRange)
  | _ ->
    None

/// A brace takes one space between it and what it fences in.
let private checkBraceSpacing src (range: range) (inner: range) (last: range) =
  if inner.StartColumn - 2 <> range.StartColumn then
    Range.mkRange "" range.Start inner.Start
    |> reportLeftCurlyBraceSpacing src
  elif last.EndColumn + 2 <> range.EndColumn then
    Range.mkRange "" last.End range.End
    |> reportRightCurlyBraceSpacing src
  else
    ()

/// A record spread over rows keeps its first field beside the opening brace
/// and its last expression beside the closing one. Where it does not, what is
/// wrong is the placement rather than the spacing, and that is asked first.
let private checkSpreadBraces src range (inner: range) (last: range) =
  if inner.StartLine <> (range: range).StartLine
    || last.EndLine <> range.EndLine then
    if isStrict then
      try
        checkBracketCompFlag src range inner last
      with
      | LintException _ ->
        (* What it found stands. Catching it here would answer a report
           with a different report. *)
        reraise ()
      | _ ->
        reportWarn src last "Move field to inline with Bracket"
    else
      ()
  else
    checkBraceSpacing src range inner last

let private checkBracketSpacingAndFormat src copyInfo fields (range: range) =
  if List.isEmpty fields then
    ()
  else
    match innerEdges copyInfo fields with
    | None ->
      ()
    | Some(inner, last) when range.StartLine = range.EndLine ->
      checkBraceSpacing src range inner last
    | Some(inner, last) ->
      checkSpreadBraces src range inner last

/// Checks spacing around '=' operator in the given source code.
/// Recursively analyzes the source for proper operator spacing.
/// Returns information about spacing issues found.
let rec private checkOperatorSpacing src = function
  | field :: rest ->
    let SynExprRecordField(equalsRange = equalsRange; expr = expr) = field
    match getFieldLastRange field, equalsRange, expr with
    | Some fieldRange, Some equalRange, Some exprRange ->
      if fieldRange.EndColumn + 1 <> equalRange.StartColumn
        && fieldRange.StartLine = exprRange.Range.StartLine then
        Range.mkRange "" fieldRange.End equalRange.Start
        |> reportEqaulBeforeSpacing src
      elif equalRange.EndColumn + 1 <> exprRange.Range.StartColumn
        && fieldRange.StartLine = exprRange.Range.StartLine then
        Range.mkRange "" equalRange.End exprRange.Range.Start
        |> reportEqaulAfterSpacing src
      else
        checkOperatorSpacing src rest
    | _ ->
      checkOperatorSpacing src rest
  | [] ->
    ()

let private collectFieldsInfo fields =
  fields
  |> List.choose (fun field ->
    let SynExprRecordField(fieldName = fieldName; expr = expr
                           blockSeparator = blockSeparator) = field
    let SynLongIdent(id = id), _ = fieldName
    if expr.IsSome then Some(id.Head.idRange, expr.Value.Range, blockSeparator)
    else None
  )

let checkSeparatorSpacing src fields =
  collectFieldsInfo fields
  |> List.iteri (fun i (_, exprRange, separatorInfo) ->
    match separatorInfo with
    | Some(separatorRange, Some _) ->
      if exprRange.EndColumn < separatorRange.StartColumn then
        Range.mkRange "" exprRange.End separatorRange.Start
        |> reportSemiColonBeforeSpacing src
      elif i < (collectFieldsInfo fields).Length - 1 then
        let fieldRange, _, _ = (collectFieldsInfo fields)[i + 1]
        if fieldRange.StartColumn - separatorRange.EndColumn <> 1
          && separatorRange.StartLine = separatorRange.EndLine
        then
          Range.mkRange "" fieldRange.Start separatorRange.End
          |> reportSemiColonAfterSpacing src
        elif fieldRange.StartColumn - separatorRange.EndColumn <> 1
          && separatorRange.StartLine <> separatorRange.EndLine
        then
          (separatorRange.Start,
           Position.mkPos separatorRange.StartLine
             (separatorRange.StartColumn + 1))
          ||> Range.mkRange ""
          |> reportTrailingSeparator src
        else
          ()
      else
        ()
    | _ ->
      ()
  )

/// The separator between two fields sharing a row: `; ` and nothing else.
let private checkPairSeparator src (front: range) (back: range) separator =
  let separator = (separator: (range * _) option).Value |> fst
  if front.EndColumn <> separator.StartColumn then
    Range.mkRange "" front.End separator.Start
    |> reportSemiColonBeforeSpacing src
  elif separator.EndColumn + 1 <> back.StartColumn then
    Range.mkRange "" separator.End back.Start
    |> reportSemiColonAfterSpacing src
  else
    ()

/// The gap two neighbouring fields leave between them. A `;` divides them
/// where they share a row and trails where they do not.
let private checkPatGap src (front: range) back frontSep backSep =
  if front.EndLine = (back: range).StartLine then
    checkPairSeparator src front back frontSep
  elif (frontSep: (range * _) option).IsSome then
    reportTrailingSeparator src (frontSep.Value |> fst)
  elif (backSep: (range * _) option).IsSome then
    reportTrailingSeparator src (backSep.Value |> fst)
  else
    ()

/// A pattern naming one field: the braces round it and the `;` it must not
/// carry.
let private checkSinglePat src topRange (field: NamePatPairField) =
  let NamePatPairField(pat = pat; range = range; blockSeparator = sepa) = field
  if sepa.IsSome then
    sepa.Value |> fst |> reportTrailingSeparator src
  else
    ()
  TypeAnnotation.checkParamTypeSpacing src pat
  checkBracketSpacing src topRange range

/// A pattern naming several: the braces round the whole, whatever trails the
/// last field, and every neighbouring pair.
let private checkManyPats (src: ISourceText)
                          (topRange: range)
                          (fieldPats: list<NamePatPairField>) =
  let startPat = List.head fieldPats
  let lastPat = List.last fieldPats
  let innerRange = Range.unionRanges startPat.Range lastPat.Range
  let gap = Range.mkRange "" innerRange.End topRange.End
  if (gap |> src.GetSubTextFromRange).Contains ';' then
    reportTrailingSeparator src gap
  else
    ()
  checkBracketSpacing src topRange innerRange
  fieldPats
  |> List.pairwise
  |> List.iter (fun (front, back) ->
    match front, back with
    | NamePatPairField(range = frontRange; blockSeparator = frontSep),
      NamePatPairField(range = backRange; blockSeparator = backSep) ->
      checkPatGap src frontRange backRange frontSep backSep
  )
  fieldPats
  |> List.iter (function
    | NamePatPairField(pat = pat) ->
      TypeAnnotation.checkParamTypeSpacing src pat
  )

let checkRecordPat (src: ISourceText) = function
  | SynPat.Record(fieldPats = fieldPats; range = range)
    when fieldPats.IsEmpty ->
    Range.mkRange "" range.Start range.End
    |> fun range -> reportWarn src range "Remove whitespace around '{}'"
  | SynPat.Record(fieldPats = fieldPats; range = topRange) ->
    AssignmentConvention.checkNamePatPairs src fieldPats
    if fieldPats.Length = 1 then checkSinglePat src topRange fieldPats.Head
    else checkManyPats src topRange fieldPats
  | _ ->
    ()

/// Checks the format of record constructors.
/// Ensures that the record fields conform to formatting conventions.
/// Every field of a record spread down the page begins a line of its own.
/// Where several share a line it is the ones after the first that have to come
/// down, and it is those the report names: the first is already where it
/// belongs, and naming it would send the reader to the wrong field.
///
/// Only a record already spread over lines is asked this. One standing on a
/// single line has chosen no layout to answer for, and nothing here asks a
/// spread record to come back up: what is wrong with a line too long is its
/// length, and the line budget says so.
let private checkSingleFieldPerLine src (range: range) (fields: range list) =
  if isStrict && range.StartLine <> range.EndLine then
    fields
    |> List.groupBy (fun (field: range) -> field.StartLine)
    |> List.iter (fun (_, fields) ->
      fields
      |> List.skip 1
      |> List.iter (reportSingleElementPerLineError src)
    )
  else
    ()

let checkConstructor src copyInfo (fields: list<SynExprRecordField>) range =
  checkBracketSpacingAndFormat src copyInfo fields range
  checkOperatorSpacing src fields
  checkSeparatorSpacing src fields
  fields |> List.choose getFieldRange |> checkSingleFieldPerLine src range

/// Checks a record definition for convention compliance.
let checkDefinition src fields range =
  checkFieldIsInlineWithBracket src range fields
  checkFieldTypeSpacing src fields

let private checkAnonymousRecordBracketSpacing src
                                               copyInfo
                                               (recordFields:
                                                 list<SynLongIdent *
                                                      range option *
                                                      SynExpr>)
                                               (range: range)
                                               (trivia: SynExprAnonRecdTrivia) =
  if not (List.isEmpty recordFields) then
    let firstInnerRange =
      match (copyInfo: option<SynExpr * _>) with
      | Some(expr, _) ->
        expr.Range
      | None ->
        let id, _, _ = List.head recordFields
        id.Range
    let _, _, lastExpr = List.last recordFields
    let lastInnerRange = (lastExpr: SynExpr).Range
    let openingRange = trivia.OpeningBraceRange
    if openingRange.EndColumn + 1 <> firstInnerRange.StartColumn
      && openingRange.StartLine = firstInnerRange.StartLine
    then
      Range.mkRange "" openingRange.Start firstInnerRange.Start
      |> reportLeftCurlyBraceSpacing src
    elif lastInnerRange.EndColumn + 3 <> range.EndColumn
      && lastInnerRange.EndLine = range.EndLine
    then
      Range.mkRange "" lastInnerRange.End range.End
      |> reportRightCurlyBraceSpacing src
    else
      ()
  else
    ()

/// The separator information is not present in a regular anonymous record
/// so it is excluded.
/// An anonymous record names its fields with a `SynLongIdent` rather than
/// wrapping them in a `SynExprRecordField`, so its names are read out of the
/// triple the parser hands over. What is asked of them is the same.
let private getAnonFieldRange (id: SynLongIdent, _, _) =
  match id with
  | SynLongIdent(id = head :: _) -> Some head.idRange
  | _ -> None

let checkAnonymousRecord src
                         copyInfo
                         (recordFields: list<SynLongIdent *
                                             range option *
                                             SynExpr>)
                         range
                         trivia =
  checkAnonymousRecordBracketSpacing src copyInfo recordFields range trivia
  recordFields
  |> List.choose getAnonFieldRange
  |> checkSingleFieldPerLine src range
  if isStrict then
    recordFields
    |> List.iter (fun (id: SynLongIdent, oprRange: option<range>, expr) ->
      if Option.isSome oprRange then
        if id.Range.EndLine = oprRange.Value.StartLine
          && oprRange.Value.StartLine = (expr: SynExpr).Range.StartLine
        then
          if id.Range.EndColumn + 1 <> oprRange.Value.StartColumn then
            Range.mkRange id.Range.FileName id.Range.End oprRange.Value.Start
            |> reportEqaulBeforeSpacing src
          elif oprRange.Value.EndColumn + 1 <> expr.Range.StartColumn then
            Range.mkRange id.Range.FileName oprRange.Value.End expr.Range.Start
            |> reportEqaulAfterSpacing src
          else
            ()
        else
          ()
      else
        ())
  else
    ()
