module B2R2.FSLint.TypeUseConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let collectRangeOfFirstAndLastType (typeArgs: SynType list) =
  typeArgs
  |> List.map (fun synType -> synType.Range)
  |> List.reduce Range.unionRanges

/// Checks that no whitespace in null type arguments.
let private checkEmpty src (typeArgsRange: range) =
  if typeArgsRange.EndColumn - typeArgsRange.StartColumn <> 3
  then
    Range.mkRange "" typeArgsRange.Start typeArgsRange.End
    |> fun range -> reportWarn src range "Remove whitespace in null type args"
  else ()

/// Checks if there is a space between 'expr' and '<type>' in an expression,
/// e.g., detects 'expr <type>' instead of the correct 'expr<type>'.
let checkFromExprToOpeningBracketSpacing src exprRange typeNameRange =
  if (exprRange: range).EndColumn <> (typeNameRange: range).StartColumn
  then
    Range.mkRange "" exprRange.End typeNameRange.Start
    |> fun range -> reportWarn src range "Remove whitespace before '<'"
  else ()

/// Checks if there is a space between '<' and 'type' or 'type' and '>' in the
/// given type argument range.
let private checkBracketSpacing src (less: range) (greater: range) elemRange =
  if less.EndLine = (elemRange: range).StartLine
    && less.EndColumn <> elemRange.StartColumn then
    Range.mkRange "" less.End elemRange.Start
    |> fun range -> reportWarn src range "Remove whitespace after '<'"
  elif greater.EndLine = elemRange.EndLine
    && greater.StartColumn <> elemRange.EndColumn then
    Range.mkRange "" elemRange.End greater.Start
    |> fun range -> reportWarn src range "Remove whitespace before '>'"
  else
    ()

let private checkStarSeparator src (typeStr: string) typeRange =
  let parts = typeStr.Split '*'
  let headEndsWithSpace = (Array.head parts).EndsWith " "
  let lastStartsWithSpace = (Array.last parts).StartsWith " "
  if not (headEndsWithSpace && lastStartsWithSpace) then
    reportWarn src typeRange "Use single whitespace around '*'"
  else
    ()

let private checkCommaSeparator src (typeStr: string) typeRange =
  let parts = typeStr.Split ','
  let lastPart = Array.last parts
  if (lastPart.Length > 0 && lastPart[0] = ' ' &&
     (lastPart.Length = 1 || lastPart[1] <> ' ')) |> not then
    reportWarn src typeRange "Use single whitespace between type"
  else
    ()
  if (Array.head parts).EndsWith " " then
    reportWarn src typeRange "Remove whitespace before ','"
  else
    ()

/// Checks whether the spacing between elements in the given type argument list
/// is formatted correctly, distinguishing between ',' and '*' separators.
let checkTypeElementSpacing src (typeArgs: SynType list) =
  let getEffectiveTypeStr (src: ISourceText) (typeRange: range) =
    let lineStr = src.GetLineString(typeRange.StartLine - 1)
    if lineStr.EndsWith "," then lineStr[0..lineStr.Length - 2]
    else lineStr
  let unionRange =
    Range.unionRanges typeArgs.Head.Range (List.last typeArgs).Range
  (typeArgs: SynType list)
  |> List.iter (fun typeArg ->
    let typeRange = typeArg.Range
    let typeStr = (src: ISourceText).GetSubTextFromRange typeRange
    if typeStr.Contains '*' then
      checkStarSeparator src typeStr unionRange
    else
      let effectiveTypeStr = getEffectiveTypeStr src typeRange
      if effectiveTypeStr.IndexOf ',' <> -1 then
        checkCommaSeparator src effectiveTypeStr unionRange
      else
        ()
  )

/// Checks if there is an unwanted space before parentheses in type application,
/// Ensures proper spacing in type application expressions.
let checkTypeAppParenSpacing src = function
  | SynExpr.App(flag = flag; funcExpr = funcExpr
                argExpr = SynExpr.Paren(leftParenRange = parenRange)) ->
    match funcExpr with
    | SynExpr.TypeApp(expr = SynExpr.LongIdent(longDotId = longDotId)
                      range = typeRange) ->
      let SynLongIdent(id = id) = longDotId
      if id.Length <> 1 then
        if flag = ExprAtomicFlag.NonAtomic
          || typeRange.EndColumn <> parenRange.StartColumn then
          Range.mkRange "" typeRange.End parenRange.Start
          |> reportPascalCaseError src
        else ()
      else () (* This handle Type Reference convention *)
    | _ -> ()
  | _ -> ()

let checkLongIdentSpacing src typeArg =
  match typeArg with
  | SynType.LongIdent(SynLongIdent(id = id)) when id.Length >= 2 ->
    id
    |> List.map _.idRange
    |> List.pairwise
    |> List.iter (fun (front, back) ->
      if front.EndColumn + 1 <> back.StartColumn then
        Range.mkRange "" front.End back.Start
        |> fun range ->
          reportWarn src range "Use single whitespace between LongIdent"
      else
        ()
    )
  | _ -> ()

let checkBracketRanges src lessRange greaterRange (innerRange: range) =
  match (lessRange: range option), (greaterRange: range option) with
  | Some lessRange, None ->
    if innerRange.StartColumn <> lessRange.EndColumn then
      Range.mkRange "" lessRange.End innerRange.Start
      |> fun range -> reportWarn src range "Remove whitespace after '<'"
    else
      ()
  | None, Some greaterRange ->
    if innerRange.EndColumn <> greaterRange.StartColumn then
      Range.mkRange "" innerRange.End greaterRange.Start
      |> fun range -> reportWarn src range "Remove whitespace before '>'"
    else
      ()
  | Some lessRange, Some greaterRange ->
    checkBracketSpacing src lessRange greaterRange innerRange
  | _ ->
    ()

let private checkBarAlignment (src: ISourceText) (range: range) = function
  | Some(barRange: range) ->
    if range.StartColumn - 2 <> barRange.StartColumn then
      if range.StartLine <> range.EndLine then
        Position.mkPos barRange.EndLine (barRange.EndColumn + 1)
        |> Range.mkRange "" barRange.Start
        |> src.GetSubTextFromRange
        |> fun str ->
          if str <> "| " then
            reportWarn src barRange "Use single whitespace after '|'"
          else
            ()
      else
        reportWarn src range "Align '|' with type"
    else
      ()
  | None -> warn "Exception: '|' range does not exist"

let checkUnionType (src: ISourceText) (cases: SynUnionCase list) =
  match cases with
  | _ when List.length cases > 1 && cases.Head.Range.StartLine <>
           (List.last cases).Range.StartLine ->
    cases
    |> List.iter (fun case ->
      let SynUnionCase(range = range; trivia = trivia) = case
      checkBarAlignment src range trivia.BarRange
    )
  | _ -> ()

let check src (expr: SynExpr) typeArgs less greater typeArgsRange =
  checkFromExprToOpeningBracketSpacing src expr.Range typeArgsRange
  collectRangeOfFirstAndLastType typeArgs
  |> checkBracketRanges src less greater
  match (typeArgs: SynType list) with
  | [] -> checkEmpty src typeArgsRange
  | _ -> checkTypeElementSpacing src typeArgs