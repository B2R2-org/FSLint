module B2R2.FSLint.AppConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let private reportInfixError src range =
  reportError src range "There must be a space before and after the infix"

let private reportIdentWithParenError src range =
  reportError src range "Need whitespace between ident and paren"

let isUnaryOperator (src: ISourceText) (operatorRange: range) =
  try
    let line = src.GetLineString(operatorRange.StartLine - 1)
    let beforeOperator = line.Substring(0, operatorRange.StartColumn).TrimEnd()
    if beforeOperator.Length > 0 then
      let lastChar = beforeOperator[beforeOperator.Length - 1]
      System.Char.IsLetterOrDigit lastChar || lastChar = ')' || lastChar = ']'
    else
      false
  with
  | _ -> false

let isOperator src = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(trivia = trivias)
                      range = range) ->
    let isOperator =
      trivias
      |> List.exists (function
        | Some(IdentTrivia.OriginalNotation "=") -> false
        | Some(IdentTrivia.OriginalNotation _) -> true
        | _ -> false
      )
    isOperator && isUnaryOperator src range
  | _ -> false

let isEquality = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(trivia = trivias)) ->
    trivias
    |> List.exists (function
      | Some(IdentTrivia.OriginalNotation "=") -> true
      | _ -> false
    )
  | _ -> false

let rec collectLastElementRange acc = function
  | SynExpr.App(argExpr = argExpr) ->
    match argExpr with
    | SynExpr.App _ -> collectLastElementRange acc argExpr
    | SynExpr.Tuple(exprs = exprs) -> (List.last exprs).Range
    | _ -> argExpr.Range
  | SynExpr.Tuple(exprs = exprs) -> (List.last exprs).Range
  | expr -> expr.Range

/// Retrieve the range of the element to the left of the operator from the given
/// source text. This is useful for analyzing expressions in the source code.
let findLeftExprFromSource (src: ISourceText) (operatorRange: range) =
  try
    let line = src.GetLineString(operatorRange.StartLine - 1)
    let beforeOperator = line.Substring(0, operatorRange.StartColumn)
    let trimmed = beforeOperator.TrimEnd()
    let leftElemEnd = trimmed.Length
    let rec findStart i =
      if i <= 0 then 0
      elif System.Char.IsWhiteSpace trimmed[i - 1] then i
      else findStart (i - 1)
    let leftElemStart = findStart leftElemEnd
    if leftElemStart < leftElemEnd then
      (Position.mkPos operatorRange.StartLine leftElemStart,
       Position.mkPos operatorRange.StartLine leftElemEnd)
      ||> Range.mkRange ""
      |> Some
    else
      None
  with
  | _ -> None

let shouldCheckFuncSpacing src funcExpr (argExpr: SynExpr) =
  let rec check = function
    | SynExpr.DotGet _ -> false
    | SynExpr.App(funcExpr = SynExpr.DotGet _) -> false
    | SynExpr.Ident _ -> true
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = first :: rest)) ->
      let names = first.idText :: List.map (fun (id: Ident) -> id.idText) rest
      if rest.Length > 0 then
        false
      else
        match names with
        | firstName :: _ when firstName.Length > 0 &&
                              System.Char.IsLower(firstName[0]) -> false
        | _ -> true
    | SynExpr.App(funcExpr = innerFunc) -> check innerFunc
    | _ -> false
  not argExpr.IsArrayOrListComputed && check funcExpr

let ensureInfixSpacing src (subFuncRange: range) (subArgRange: range) argRange =
  if (argRange: range).StartLine = subFuncRange.StartLine then
    if argRange.StartColumn <> subFuncRange.EndColumn + 1 then
      reportInfixError src argRange
    else ()
  else ()
  if subArgRange.StartLine = subFuncRange.StartLine then
    if subFuncRange.StartColumn <> subArgRange.EndColumn + 1 then
      reportInfixError src subFuncRange
    else ()
  else ()

let ensureAddressOfSpacing src (exprRange: range) (opRange: range) =
  if opRange.StartLine = exprRange.StartLine then
    if opRange.EndColumn <> exprRange.StartColumn then
      reportInfixError src opRange
    else ()
  else ()

let ensureFuncSpacing src (funcRange: range) (argRange: range) =
  if (argRange.StartColumn - funcRange.EndColumn <> 1
    && funcRange.StartLine = argRange.StartLine)
    && isUnaryOperator src funcRange |> not
  then reportError src argRange "Func app must be separated by a single space."
  else ()

/// Validates spacing around (=) operators ensuring proper whitespace.
/// Checks for exact spacing requirements in assignment expressions.
let checkAssignSpacing src (subFuncRange: range) (subArgRange: range) argRange =
  if subFuncRange.StartColumn <> subArgRange.EndColumn + 1
    || subFuncRange.EndColumn <> (argRange: range).StartColumn - 1
  then reportError src subFuncRange "Contains invalid whitespace"
  else ()

/// Check proper spacing in infix applications.
/// Ensures a single space from the operator to each applied element
/// (e.g., "a + b", not "a+b").
let rec checkInfixSpacing src isInfix funcExpr (argExpr: SynExpr) =
  match funcExpr with
  | SynExpr.App(isInfix = isInfixInner
                funcExpr = subFuncExpr
                argExpr = subArgExpr) ->
    if (isInfix || isInfixInner) && isOperator src subFuncExpr then
      ensureInfixSpacing src subFuncExpr.Range subArgExpr.Range argExpr.Range
    elif isEquality subFuncExpr then
      checkAssignSpacing src subFuncExpr.Range subArgExpr.Range argExpr.Range
    else ()
    match subArgExpr with
    | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
      checkInfixSpacing src isInfix funcExpr argExpr
    | _ -> ()
  | _ -> ()
  match argExpr with
  | SynExpr.App(isInfix = isInfixInner
                funcExpr = subFuncExpr
                argExpr = subArgExpr) ->
    if isOperator src subFuncExpr then
      let leafArgRange = findLeftExprFromSource src subFuncExpr.Range
      if leafArgRange.IsSome then
        let subArgRange = collectLastElementRange subArgExpr.Range subArgExpr
        ensureInfixSpacing src subFuncExpr.Range leafArgRange.Value subArgRange
      else ()
    else ()
    checkInfixSpacing src (isInfixInner || isOperator src subFuncExpr)
      subFuncExpr subArgExpr
  | SynExpr.AddressOf(expr = expr; opRange = opRange) ->
    ensureAddressOfSpacing src expr.Range opRange
  | _ -> ()

/// Check proper spacing in function applications.
/// Ensures single space between each applied element
/// (e.g., "fn 1 2", not "fn  1  2").
let rec checkFuncSpacing src (funcExpr: SynExpr) (argExpr: SynExpr) =
  if not argExpr.IsArrayOrListComputed || argExpr.IsParen then
    match funcExpr with
    | SynExpr.App(funcExpr = subFuncExpr; argExpr = subArgExpr) ->
      ensureFuncSpacing src funcExpr.Range argExpr.Range
      checkFuncSpacing src subFuncExpr subArgExpr
    | SynExpr.Ident _ | SynExpr.LongIdent _ ->
      ensureFuncSpacing src funcExpr.Range argExpr.Range
    | SynExpr.Paren _ | SynExpr.DotGet _ -> ()
    | _ ->
      warn $"[checkFuncAppSpacing]TODO: {funcExpr}"
  else ()
  match funcExpr with
  | SynExpr.App(isInfix = false; funcExpr = innerFunc; argExpr = innerArg) ->
    checkFuncSpacing src innerFunc innerArg
  | _ -> ()

/// Validates spacing between identifiers and parentheses.
/// Ensures exactly one space between identifier and opening parenthesis.
/// Handles atomic expressions and nested applications appropriately.
let checkIdentWithParenSpacing src flag (ident: Ident) argExpr check =
  match argExpr with
  | SynExpr.Paren(expr = innerExpr) ->
    match innerExpr with
    | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
      check src isInfix flag funcExpr argExpr
    | _ -> ()
  | _ -> ()

let checkInfixOrFuncSpacing src isInfix funcExpr argExpr =
  if isInfix then checkInfixSpacing src isInfix funcExpr argExpr
  elif isOperator src funcExpr || shouldCheckFuncSpacing src funcExpr argExpr
  then checkFuncSpacing src funcExpr argExpr
  else ()

let rec check src isInfix flag funcExpr (argExpr: SynExpr) =
  match funcExpr with
  | SynExpr.App(isInfix = subIsInfix
                funcExpr = subFuncExpr
                argExpr = subArgExpr) ->
    checkInfixOrFuncSpacing src subIsInfix funcExpr argExpr
    check src subIsInfix flag subFuncExpr subArgExpr
  | SynExpr.LongIdent _ ->
    checkInfixOrFuncSpacing src isInfix funcExpr argExpr
  | SynExpr.Paren(expr = innerExpr) ->
    match innerExpr with
    | SynExpr.App(flag = flag; funcExpr = subFuncExpr; argExpr = subArgExpr) ->
      check src isInfix flag subFuncExpr subArgExpr
    | _ -> ()
  | SynExpr.Ident(ident = ident) ->
    checkIdentWithParenSpacing src flag ident argExpr check
  | SynExpr.TypeApp _ | SynExpr.DotGet _ | SynExpr.DotLambda _ -> ()
  | expr -> warn $"[AppConvention] TODO: {expr}"
  match argExpr with
  | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
    check src (isInfix || isOperator src funcExpr) flag funcExpr argExpr
  | SynExpr.Paren(expr = innerExpr) ->
    match innerExpr with
    | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
      check src isInfix flag funcExpr argExpr
    | _ -> ()
  | SynExpr.AddressOf _ ->
    checkInfixSpacing src true funcExpr argExpr
  | _ -> ()