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

let checkTypeAbbrev src = function
  | SynType.App(lessRange = lessRange
                typeArgs = typeArgs
                greaterRange = greaterRange) ->
    let innerRange = collectRangeOfFirstAndLastType typeArgs
    checkBracketRanges src innerRange lessRange greaterRange
    typeArgs |> List.iter (checkLongIdentSpacing src)
    checkTypeElementSpacing src typeArgs
  | _ -> ()

let check src expr (typeArgs: SynType list) typeArgsRange =
  checkFromExprToOpeningBracketSpacing src expr typeArgsRange
  match typeArgs with
  | [] -> checkEmpty src typeArgsRange
  | _ ->
    collectRangeOfFirstAndLastType typeArgs
    |> checkBracketSpacing src typeArgsRange
    checkTypeElementSpacing src typeArgs