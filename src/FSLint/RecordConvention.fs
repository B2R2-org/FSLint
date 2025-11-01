module B2R2.FSLint.RecordConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private getFieldRange (SynExprRecordField(fieldName = fieldName)) =
  match fst fieldName with
  | SynLongIdent(id = id) -> Some id.Head.idRange

let private getFieldLastRange (SynExprRecordField(fieldName = fieldName)) =
  match fst fieldName with
  | SynLongIdent(id = id) -> Some <| (List.last id).idRange

let private getExprRange (SynExprRecordField(expr = expr)) =
  expr |> Option.map (fun e -> e.Range)

/// Checks for correct spacing around ':' in record field definitions.
/// Ensures the format `Field: type`.
let checkFieldTypeSpacing (src: ISourceText) fields =
  List.iter (fun field ->
    let SynField(idOpt = idOpt; fieldType = fieldType; range = range) = field
    if idOpt.IsSome then
      (Position.mkPos range.EndLine idOpt.Value.idRange.EndColumn,
       Position.mkPos range.EndLine fieldType.Range.StartColumn)
      ||> Range.mkRange ""
      |> fun colonRange ->
        if src.GetSubTextFromRange colonRange <> ": " then
          reportError src colonRange "Contains invalid whitespace."
    else ()
  ) fields

/// Checks that the '=' and '{' in a record definition are on the same line.
/// Ensures record definitions follow the convention: `type T = { ... }`
let checkOpeningBracketPosition src (range: range) (trivia: SynTypeDefnTrivia) =
  if trivia.EqualsRange.IsSome && range.StartLine <> range.EndLine then
    if trivia.EqualsRange.Value.StartLine = range.StartLine then
      reportError src range "Opening Bracket not inline with equal operator"
    else
      ()
  else ()

let checkFieldCompFlag (src: ISourceText) (range: range) (innerRange: range) =
  (Position.mkPos (innerRange.EndLine + 1) 0,
   Position.mkPos range.EndLine 0)
  ||> Range.mkRange ""
  |> src.GetSubTextFromRange
  |> fun subStr ->
    subStr.Split([| '\n' |], StringSplitOptions.None)
    |> fun strArr ->
      let flagStartIsWrong =
        (Array.head strArr).TrimStart().StartsWith "#if" |> not
      let flagEndIsWrong = Array.last strArr |> String.IsNullOrEmpty |> not
      if flagEndIsWrong || flagStartIsWrong then
        reportError src innerRange "Field not inline with bracket."
      else
        ()

let checkFieldIsInlineWithBracket src (range: range) fields =
  fields
  |> List.map (fun (SynField(range = range)) -> range)
  |> List.reduce Range.unionRanges
  |> fun innerRange ->
    if range.StartLine <> innerRange.StartLine
      || range.EndLine <> innerRange.EndLine
    then
      try checkFieldCompFlag src range innerRange
      with _ -> reportError src innerRange "Field not inline with bracket."
    elif range.StartColumn + 2 <> innerRange.StartColumn
      || range.EndColumn - 2 <> innerRange.EndColumn
    then reportError src range "Wrong spacing inside brackets"
    else ()

let checkBracketCompFlag src (fullRange: range) (fieldRange: range) exprRange =
  if fullRange.StartLine <> fieldRange.StartLine then
    reportError src fieldRange "Opening Bracket not inline with equal operator"
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
        reportError src fullRange "Bracket should inline with record field."
      else
        ()

/// Checks for correct spacing and formatting in record field assignments,
/// ensuring the format `{ field = expr }` is used instead of `{field = expr}`.
/// Also validates bracket positioning and formatting in multi-line records.
let checkBracketSpacingAndFormat src copyInfo fields (range: range) =
  if range.StartLine = range.EndLine && not (List.isEmpty fields) then
    match getFieldRange (List.head fields), getExprRange (List.last fields) with
    | Some fieldRange, Some exprRange ->
      let fieldRange =
        match (copyInfo: option<SynExpr * _>) with
        | Some(expr, _) -> expr.Range
        | _ -> fieldRange
      if fieldRange.StartColumn - 2 <> range.StartColumn
        || exprRange.EndColumn + 2 <> range.EndColumn
      then reportError src range "Wrong spacing inside brackets"
    | _ -> ()
  elif range.StartLine <> range.EndLine && not (List.isEmpty fields) then
    match getFieldRange (List.head fields), getExprRange (List.last fields) with
    | Some fieldRange, Some exprRange ->
      let fieldRange =
        match (copyInfo: option<SynExpr * _>) with
        | Some(expr, _) -> expr.Range
        | _ -> fieldRange
      if fieldRange.StartLine <> range.StartLine
        || exprRange.EndLine <> range.EndLine
      then
        try
          checkBracketCompFlag src range fieldRange exprRange
        with _ ->
          reportError src exprRange "Bracket should inline with record field."
      elif fieldRange.StartColumn - 2 <> range.StartColumn
        || exprRange.EndColumn + 2 <> range.EndColumn
      then reportError src range "Wrong spacing inside brackets"
      else ()
    | _ -> ()
  else
    ()

/// Checks spacing around '=' operator in the given source code.
/// Recursively analyzes the source for proper operator spacing.
/// Returns information about spacing issues found.
let rec checkOperatorSpacing src = function
  | field :: rest ->
    let SynExprRecordField(equalsRange = equalsRange; expr = expr) = field
    match getFieldLastRange field, equalsRange, expr with
    | Some fieldRange, Some equalRange, Some exprRange ->
      if fieldRange.EndColumn + 1 <> equalRange.StartColumn ||
        equalRange.EndColumn + 1 <> exprRange.Range.StartColumn &&
        fieldRange.StartLine = exprRange.Range.StartLine
      then
        reportError src fieldRange "Need single space around '='"
      else
        checkOperatorSpacing src rest
    | _ -> checkOperatorSpacing src rest
  | [] -> ()

let collectFieldsInfo fields =
  fields
  |> List.choose (fun field ->
    let SynExprRecordField(fieldName = fieldName; expr = expr
                           blockSeparator = blockSeparator) = field
    let SynLongIdent(id = id), _ = fieldName
    if expr.IsSome then
      Some(id.Head.idRange, expr.Value.Range, blockSeparator)
    else
      None
  )

let checkSeparatorSpacing src fields =
  collectFieldsInfo fields
  |> List.iteri (fun i (_, exprRange, separatorInfo) ->
    match separatorInfo with
    | Some(separatorRange, Some _) ->
      if exprRange.EndColumn < separatorRange.StartColumn then
        reportError src exprRange "No space before semicolon"
      elif i < (collectFieldsInfo fields).Length - 1 then
        let fieldRange, _, _ = (collectFieldsInfo fields)[i + 1]
        let spaceAfterSemicolon =
          fieldRange.StartColumn - separatorRange.EndColumn
        if spaceAfterSemicolon <> 1 then
          reportError src separatorRange "Need single space after separator."
        else
          ()
      else
        ()
    | _ -> ()
  )

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