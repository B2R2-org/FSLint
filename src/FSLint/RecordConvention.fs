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
  else ()

/// Checks for correct spacing around ':' in record field definitions.
/// Ensures the format `Field: type`.
let private checkFieldTypeSpacing (src: ISourceText) fields =
  List.iter (fun field ->
    let SynField(idOpt = idOpt; fieldType = fieldType; range = range) = field
    if idOpt.IsSome then
      (Position.mkPos range.EndLine idOpt.Value.idRange.EndColumn,
       Position.mkPos range.EndLine fieldType.Range.StartColumn)
      ||> Range.mkRange ""
      |> fun colonRange ->
        if src.GetSubTextFromRange colonRange <> ": " then
          reportWarn src colonRange "Use ': ' between field and type"
        else
          ()
    else ()
  ) fields

/// Checks that the '=' and '{' in a record definition are on the same line.
/// Ensures record definitions follow the convention: `type T = { ... }`
let private checkOpeningBracketPosition src range (trivia: SynTypeDefnTrivia) =
  if trivia.EqualsRange.IsSome && (range: range).StartLine <> range.EndLine then
    if trivia.EqualsRange.Value.StartLine = range.StartLine then
      (src: ISourceText).GetLineString(range.StartLine - 1).Length
      |> fun lastIdx ->
        Range.mkRange "" range.Start (Position.mkPos range.StartLine lastIdx)
      |> fun wRange -> reportWarn src wRange "Move '{' to next line"
    else
      ()
  else ()

let private checkFieldCompFlag src fullRange innerRange ranges isOpenBracket =
  (Position.mkPos ((innerRange: range).EndLine + 1) 0,
   Position.mkPos (fullRange: range).EndLine 0)
  ||> Range.mkRange ""
  |> (src: ISourceText).GetSubTextFromRange
  |> fun subStr ->
    subStr.Split([| '\n' |], StringSplitOptions.None)
    |> fun strArr ->
      let flagStartIsWrong =
        (Array.head strArr).TrimStart().StartsWith "#if" |> not
      let flagEndIsWrong = Array.last strArr |> String.IsNullOrEmpty |> not
      flagEndIsWrong || flagStartIsWrong

let private checkFieldIsInlineWithBracket src (fullRange: range) fields =
  fields
  |> List.map (fun (SynField(range = range)) -> range)
  |> fun ranges ->
    ranges
    |> List.reduce Range.unionRanges
    |> fun innerRange ->
      let isOpenBracket = fullRange.StartLine <> innerRange.StartLine
      if fullRange.StartLine <> innerRange.StartLine
        || fullRange.EndLine <> innerRange.EndLine
        && checkFieldCompFlag src fullRange innerRange ranges isOpenBracket
      then
        Range.mkRange "" (Position.mkPos fullRange.EndLine 0) fullRange.End
        |> fun wRange -> reportWarn src wRange "Move to inline with bracket"
      elif fullRange.StartColumn + 2 <> innerRange.StartColumn
      then
        Range.mkRange "" fullRange.Start innerRange.Start
        |> reportLeftCurlyBraceSpacing src
      elif fullRange.EndColumn - 2 <> innerRange.EndColumn
      then
        Range.mkRange "" innerRange.End fullRange.End
        |> reportRightCurlyBraceSpacing src
      else ()

let private checkBracketCompFlag src fullRange fieldRange exprRange =
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

/// Checks for correct spacing and formatting in record field assignments,
/// ensuring the format `{ field = expr }` is used instead of `{field = expr}`.
/// Also validates bracket positioning and formatting in multi-line records.
let private checkBracketSpacingAndFormat src copyInfo fields (range: range) =
  if range.StartLine = range.EndLine && not (List.isEmpty fields) then
    match getFieldRange (List.head fields), getExprRange (List.last fields) with
    | Some fieldRange, Some exprRange ->
      let fieldRange =
        match (copyInfo: option<SynExpr * _>) with
        | Some(expr, _) -> expr.Range
        | _ -> fieldRange
      if fieldRange.StartColumn - 2 <> range.StartColumn then
        Range.mkRange "" range.Start fieldRange.Start
        |> reportLeftCurlyBraceSpacing src
      elif exprRange.EndColumn + 2 <> range.EndColumn then
        Range.mkRange "" exprRange.End range.End
        |> reportRightCurlyBraceSpacing src
      else ()
    | _ -> ()
  elif range.StartLine <> range.EndLine && not (List.isEmpty fields) then
    match getFieldRange (List.head fields), getExprRange (List.last fields) with
    | Some fieldRange, Some exprRange ->
      let fieldRange =
        match (copyInfo: option<SynExpr * _>) with
        | Some(expr, _) -> expr.Range
        | _ -> fieldRange
      if fieldRange.StartLine <> range.StartLine
        || exprRange.EndLine <> range.EndLine then
        try
          checkBracketCompFlag src range fieldRange exprRange
        with _ ->
          reportWarn src exprRange "Move field to inline with Bracket"
      elif fieldRange.StartColumn - 2 <> range.StartColumn then
        Range.mkRange "" range.Start fieldRange.Start
        |> reportLeftCurlyBraceSpacing src
      elif exprRange.EndColumn + 2 <> range.EndColumn then
        Range.mkRange "" exprRange.End range.End
        |> reportRightCurlyBraceSpacing src
      else
        ()
    | _ -> ()
  else
    ()

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
    | _ -> checkOperatorSpacing src rest
  | [] -> ()

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
        else ()
      else
        ()
    | _ -> ()
  )

let checkRecordPat (src: ISourceText) = function
  | SynPat.Record(fieldPats = fieldPats; range = range)
    when fieldPats.IsEmpty ->
    Range.mkRange "" range.Start range.End
    |> fun range -> reportWarn src range "Remove whitespace around '{}'"
  | SynPat.Record(fieldPats = fieldPats; range = topRange) ->
    AssignmentConvention.checkNamePatPairs src fieldPats
    if fieldPats.Length = 1 then
      let NamePatPairField(pat = pat
                           range = range
                           blockSeparator = sepa) = fieldPats.Head
      if sepa.IsSome then sepa.Value |> fst |> reportTrailingSeparator src
      else ()
      TypeAnnotation.checkParamTypeSpacing src pat
      checkBracketSpacing src topRange range
    else
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
      |> List.iter (fun pairs ->
        match pairs with
        | NamePatPairField(pat = frontPat
                           range = frontRange
                           blockSeparator = frontSeparator),
          NamePatPairField(pat = backPat
                           range = backRange
                           blockSeparator = backSeparator) ->
          if frontRange.EndLine <> backRange.StartLine
            && frontSeparator.IsSome then
            reportTrailingSeparator src (frontSeparator.Value |> fst)
          elif frontRange.EndLine <> backRange.StartLine
            && backSeparator.IsSome then
            reportTrailingSeparator src (backSeparator.Value |> fst)
          else
            ()
          if frontRange.EndLine = backRange.StartLine then
            let frontSeparator = frontSeparator.Value |> fst
            if frontRange.EndColumn <> frontSeparator.StartColumn then
              Range.mkRange "" frontRange.End frontSeparator.Start
              |> reportSemiColonBeforeSpacing src
            elif frontSeparator.EndColumn + 1 <> backRange.StartColumn then
              Range.mkRange "" frontSeparator.End backRange.Start
              |> reportSemiColonAfterSpacing src
            else
              ()
          else
            ()
          TypeAnnotation.checkParamTypeSpacing src frontPat
          TypeAnnotation.checkParamTypeSpacing src backPat
      )
      fieldPats
      |> List.iter (function
       | NamePatPairField(pat = pat; blockSeparator = blockSeparator) ->
         TypeAnnotation.checkParamTypeSpacing src pat
      )
  | _ ->
    ()

/// Checks the format of record constructors.
/// Ensures that the record fields conform to formatting conventions.
let checkConstructor src copyInfo (fields: list<SynExprRecordField>) range =
  checkBracketSpacingAndFormat src copyInfo fields range
  checkOperatorSpacing src fields
  checkSeparatorSpacing src fields

/// Checks a record definition for convention compliance.
let checkDefinition src fields range trivia =
  checkOpeningBracketPosition src range trivia
  checkFieldIsInlineWithBracket src range fields
  checkFieldTypeSpacing src fields