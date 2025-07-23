module B2R2.FSLint.RecordConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private getFieldRange (SynExprRecordField(fieldName = fieldName)) =
  match fst fieldName with
  | SynLongIdent(id = id) -> Some id.Head.idRange

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
  if trivia.EqualsRange.IsSome then
    if trivia.EqualsRange.Value.StartLine <> range.StartLine then
      reportError src range "Opening Bracket not inline with equal operator"
    else ()
  else ()

/// Checks for correct spacing and formatting in record field assignments,
/// ensuring the format `{ field = expr }` is used instead of `{field = expr}`.
/// Also validates bracket positioning and formatting in multi-line records.
let checkBracketSpacingAndFormat src fields (range: range) =
  if range.StartLine = range.EndLine && not (List.isEmpty fields) then
    match getFieldRange (List.head fields), getExprRange (List.last fields) with
    | Some fieldRange, Some exprRange ->
      if fieldRange.StartColumn - 2 <> range.StartColumn
        || exprRange.EndColumn + 2 <> range.EndColumn
      then reportError src range "Wrong spacing inside brackets"
    | _ -> ()
  elif range.StartLine <> range.EndLine && not (List.isEmpty fields) then
    match getFieldRange (List.head fields), getExprRange (List.last fields) with
    | Some fieldRange, Some exprRange ->
      if fieldRange.StartLine <> range.StartLine
        || exprRange.StartLine <> range.EndLine
      then reportError src range "Bracket should inline with record field."
      elif fieldRange.StartColumn - 2 <> range.StartColumn
        || exprRange.EndColumn + 2 <> range.EndColumn
      then reportError src range "Wrong spacing inside brackets"
      else ()
    | _ -> ()
  else ()

/// Checks spacing around '=' operator in the given source code.
/// Recursively analyzes the source for proper operator spacing.
/// Returns information about spacing issues found.
let rec checkOperatorSpacing src = function
  | field :: rest ->
    let SynExprRecordField(equalsRange = equalsRange; expr = expr) = field
    match getFieldRange field, equalsRange, expr with
     | Some fieldRange, Some equalRange, Some exprRange ->
       if fieldRange.EndColumn + 1 <> equalRange.StartColumn
         || equalRange.EndColumn + 1 <> exprRange.Range.StartColumn
       then reportError src fieldRange "Need single space around '='"
       else ()
     | _ -> ()
    checkOperatorSpacing src rest
  | [] -> ()

let checkSeparatorSpacing src fields =
  let infos =
    fields
    |> List.choose (fun field ->
      let SynExprRecordField(fieldName = fieldName
                             expr = expr
                             blockSeparator = blockSeparator) = field
      let SynLongIdent(id = id), _ = fieldName
      if expr.IsSome then
        Some(id.Head.idRange, expr.Value.Range, blockSeparator)
      else
        None
    )
  infos
  |> List.iteri (fun i (_, exprRange, separatorInfo) ->
    match separatorInfo with
    | Some(separatorRange, Some _) ->
      if exprRange.EndColumn < separatorRange.StartColumn then
        reportError src exprRange "No space before semicolon"
      elif i < infos.Length - 1 then
        let fieldRange, _, _ = infos[i + 1]
        let spaceAfterSemicolon =
          fieldRange.StartColumn - separatorRange.EndColumn
        if spaceAfterSemicolon <> 1 then
          reportError src separatorRange "Need single space after separator."
      else ()
    | _ -> ()
  )

/// Checks the format of record constructors.
/// Ensures that the record fields conform to formatting conventions.
let checkConstructor src (fields: list<SynExprRecordField>) range =
  checkBracketSpacingAndFormat src fields range
  checkOperatorSpacing src fields
  checkSeparatorSpacing src fields

/// Checks a record definition for convention compliance.
let checkDefinition src fields range trivia =
  checkOpeningBracketPosition src range trivia
  checkFieldTypeSpacing src fields