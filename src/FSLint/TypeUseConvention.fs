module B2R2.FSLint.TypeUseConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let private collectRangeOfFirstAndLastType (typeArgs: SynType list) =
  typeArgs
  |> List.map (fun synType -> synType.Range)
  |> List.reduce Range.unionRanges

/// Checks that no whitespace in null type arguments.
let private checkEmpty src (typeArgsRange: range) =
  if typeArgsRange.EndColumn - typeArgsRange.StartColumn <> 3
  then reportWarn src typeArgsRange "Remove whitespace in null type args"
  else ()

/// Checks if there is a space between 'expr' and '<type>' in an expression,
/// e.g., detects 'expr <type>' instead of the correct 'expr<type>'.
let private checkFromExprToOpeningBracketSpacing src expr typeArgsRange =
  if (expr: SynExpr).Range.EndColumn <> (typeArgsRange: range).StartColumn
  then reportWarn src typeArgsRange "Remove whitespace before '<'"
  else ()

/// Checks if there is a space between '<' and 'type' or 'type' and '>' in the
/// given type argument range.
let private checkBracketSpacing src typeArgsRange edgeRange =
  if (edgeRange: range).StartColumn - 1 <> (typeArgsRange: range).StartColumn
    || edgeRange.EndColumn + 1 <> typeArgsRange.EndColumn
  then reportWarn src edgeRange "Remove whitespace around bracket"
  else ()

let private checkStarSeparator src (typeStr: string) typeRange =
  let parts = typeStr.Split '*'
  let headEndsWithSpace = (Array.head parts).EndsWith " "
  let lastStartsWithSpace = (Array.last parts).StartsWith " "
  if not (headEndsWithSpace && lastStartsWithSpace) then
    reportWarn src typeRange "Use single whitespace between type"
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
let private checkTypeElementSpacing src (typeArgs: SynType list) =
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

let private checkLongIdentSpacing src typeArg =
  match typeArg with
  | SynType.LongIdent(SynLongIdent(id = id)) when id.Length >= 2 ->
    id
    |> List.map (_.idRange)
    |> List.pairwise
    |> List.iter (fun (front, back) ->
      if front.EndColumn + 1 <> back.StartColumn then
        reportWarn src back "Use single whitespace between LongIdent"
      else
        ()
    )
  | _ -> ()

let private checkBracketRanges src (innerRange: range) lessRange greaterRange =
  match (lessRange: range option), (greaterRange: range option) with
  | Some lessRange, None ->
    if innerRange.StartColumn <> lessRange.EndColumn then
      reportWarn src lessRange "Remove whitespace after inner"
    else
      ()
  | None, Some greaterRange ->
    if innerRange.EndColumn <> greaterRange.StartColumn then
      reportWarn src greaterRange "Remove whitespace after inner"
    else
      ()
  | Some lessRange, Some greaterRange ->
    let range = Range.unionRanges lessRange greaterRange
    checkBracketSpacing src range innerRange
  | _ ->
    ()

let rec checkTypeAbbrevWithAnnotation src = function
  | SynType.App(lessRange = lessRange
                typeArgs = typeArgs
                greaterRange = greaterRange) ->
    let innerRange = collectRangeOfFirstAndLastType typeArgs
    checkBracketRanges src innerRange lessRange greaterRange
    typeArgs |> List.iter (checkLongIdentSpacing src)
    checkTypeElementSpacing src typeArgs
  | SynType.Fun(argType = argType; returnType = returnType; trivia = trivia) ->
    if argType.Range.EndLine <> trivia.ArrowRange.StartLine
      && returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn
    then reportWarn src trivia.ArrowRange "Use single whitespace after '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine = returnType.Range.StartLine
      && (argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
      || returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn)
    then reportWarn src trivia.ArrowRange "Use single whitespace around '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine <> returnType.Range.StartLine
      && argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
    then reportWarn src trivia.ArrowRange "Use single whitespace before '->'"
    else ()
    TypeAnnotation.checkFieldWidth src argType.Range
    TypeAnnotation.checkFieldWidth src returnType.Range
    checkTypeAbbrevWithAnnotation src argType
    checkTypeAbbrevWithAnnotation src returnType
  | _ -> ()

let checkExprAnnotation src = function
  | SynExpr.Typed(targetType = targetType) ->
    checkTypeAbbrevWithAnnotation src targetType
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

let rec checkParamTypeSpacing src = function
  | SynPat.LongIdent(argPats = SynArgPats.NamePatPairs(pats = pats)) ->
    pats |> List.iter (fun field -> checkParamTypeSpacing src field.Pattern)
  | SynPat.LongIdent(argPats = SynArgPats.Pats pats) ->
    pats |> List.iter (checkParamTypeSpacing src)
  | SynPat.Tuple(elementPats = pats) ->
    pats |> List.iter (checkParamTypeSpacing src)
  | SynPat.Paren(pat, range) ->
    TypeAnnotation.checkFieldWidth src range
    checkParamTypeSpacing src pat
  | SynPat.Typed(pat, targetType, range) ->
    checkTypeAbbrevWithAnnotation src targetType
    TypeAnnotation.checkFieldWidth src range
    checkParamTypeSpacing src pat
  | SynPat.As(lhsPat = lhsPat; rhsPat = rhsPat) ->
    checkParamTypeSpacing src lhsPat
    checkParamTypeSpacing src rhsPat
  | SynPat.IsInst(pat = pat) ->
    checkTypeAbbrevWithAnnotation src pat
  | SynPat.Named(range = range) ->
    TypeAnnotation.checkFieldWidth src range
  | _ ->
    ()

let check src expr (typeArgs: SynType list) typeArgsRange =
  checkFromExprToOpeningBracketSpacing src expr typeArgsRange
  match typeArgs with
  | [] -> checkEmpty src typeArgsRange
  | _ ->
    collectRangeOfFirstAndLastType typeArgs
    |> checkBracketSpacing src typeArgsRange
    checkTypeElementSpacing src typeArgs