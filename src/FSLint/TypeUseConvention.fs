module B2R2.FSLint.TypeUseConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let collectRangeOfFirstAndLastType (typeArgs: SynType list) =
  typeArgs
  |> List.map (fun synType -> synType.Range)
  |> List.reduce Range.unionRanges

/// Checks that no whitespace in null type arguments.
let checkEmpty src (typeArgsRange: range) =
  if typeArgsRange.EndColumn - typeArgsRange.StartColumn <> 3 then
    reportError src typeArgsRange "Contains invalid whitespace"
  else ()

/// Checks if there is a space between 'expr' and '<type>' in an expression,
/// e.g., detects 'expr <type>' instead of the correct 'expr<type>'.
let checkFromExprToOpeningBracketSpacing src expr typeArgsRange =
  if (expr: SynExpr).Range.EndColumn <> (typeArgsRange: range).StartColumn then
    reportError src typeArgsRange "Contains invalid whitespace"
  else ()

/// Checks if there is a space between '<' and 'type' or 'type' and '>' in the
/// given type argument range.
let checkBracketSpacing src typeArgsRange edgeRange =
  if (edgeRange: range).StartColumn - 1 <> (typeArgsRange: range).StartColumn
    || edgeRange.EndColumn + 1 <> typeArgsRange.EndColumn
  then reportError src edgeRange "Contains invalid whitespace"
  else ()

let checkStarSeparator src (typeStr: string) typeRange =
  let parts = typeStr.Split '*'
  let headEndsWithSpace = (Array.head parts).EndsWith " "
  let lastStartsWithSpace = (Array.last parts).StartsWith " "
  if not (headEndsWithSpace && lastStartsWithSpace) then
    reportError src typeRange "Need single space between type."
  else
    ()

let checkCommaSeparator src (typeStr: string) typeRange =
  let parts = typeStr.Split ','
  let lastPart = Array.last parts
  if (lastPart.Length > 0 && lastPart[0] = ' ' &&
     (lastPart.Length = 1 || lastPart[1] <> ' ')) |> not then
    reportError src typeRange "Need single space between type."
  else
    ()
  if (Array.head parts).EndsWith " " then
    reportError src typeRange "No space allowed before comma."
  else
    ()

/// Checks whether the spacing between elements in the given type argument list
/// is formatted correctly, distinguishing between ',' and '*' separators.
let checkTypeElementSpacing (src: ISourceText) (typeArgs: SynType list) =
  let getEffectiveTypeStr (src: ISourceText) (typeRange: range) =
    let lineStr = src.GetLineString(typeRange.StartLine - 1)
    if lineStr.EndsWith "," then lineStr[0..lineStr.Length - 2]
    else lineStr
  typeArgs
  |> List.iter (fun typeArg ->
    let typeRange = typeArg.Range
    let typeStr = src.GetSubTextFromRange typeRange
    if typeStr.Contains '*' then
      checkStarSeparator src typeStr typeRange
    else
      let effectiveTypeStr = getEffectiveTypeStr src typeRange
      if effectiveTypeStr.IndexOf ',' <> -1 then
        checkCommaSeparator src effectiveTypeStr typeRange
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
          reportError src parenRange "Contains invalid whitespace"
        else ()
      else () (* This handle Type Reference convention *)
    | _ -> ()
  | _ -> ()

let checkLongIdentSpacing src typeArg =
  match typeArg with
  | SynType.LongIdent(SynLongIdent(id = id)) when id.Length >= 2 ->
    id
    |> List.map (_.idRange)
    |> List.pairwise
    |> List.iter (fun (front, back) ->
      if front.EndColumn + 1 <> back.StartColumn then
        reportError src back "Contains invalid whitespace"
      else
        ()
    )
  | _ -> ()

let checkBracketRanges src (innerRange: range) lessRange greaterRange =
  match (lessRange: range option), (greaterRange: range option) with
  | Some lessRange, None ->
    if innerRange.StartColumn <> lessRange.EndColumn then
      reportError src lessRange "Contains invalid whitespace"
    else
      ()
  | None, Some greaterRange ->
    if innerRange.EndColumn <> greaterRange.StartColumn then
      reportError src greaterRange "Contains invalid whitespace"
    else
      ()
  | Some lessRange, Some greaterRange ->
    let range = Range.unionRanges lessRange greaterRange
    checkBracketSpacing src range innerRange
  | _ -> ()

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
    then reportError src trivia.ArrowRange "Contains invalid whitespace"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine = returnType.Range.StartLine
      && (argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
      || returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn)
    then reportError src trivia.ArrowRange "Contains invalid whitespace"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine <> returnType.Range.StartLine
      && argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
    then reportError src trivia.ArrowRange "Contains invalid whitespace"
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

let checkBarAlignment (src: ISourceText) (range: range) = function
  | Some(barRange: range) ->
    if range.StartColumn - 2 <> barRange.StartColumn then
      if range.StartLine <> range.EndLine then
        Position.mkPos barRange.EndLine (barRange.EndColumn + 1)
        |> Range.mkRange "" barRange.Start
        |> src.GetSubTextFromRange
        |> fun str ->
          if str <> "| " then
            reportError src barRange "Contains invalid whitespace"
          else
            ()
      else
        reportError src range "Contains invalid whitespace"
    else
      ()
  | None ->
    warn "Union case does not have a bar range."

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
    let _, _, pats = List.unzip3 pats
    pats |> List.iter (checkParamTypeSpacing src)
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