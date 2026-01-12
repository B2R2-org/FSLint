module B2R2.FSLint.AppConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let private generalOperator =
  [ "+"
    "-"
    "*"
    "/"
    "%"
    "**"
    "="
    "<>"
    "<"
    ">"
    "<="
    ">="
    "&&"
    "||"
    "&&&"
    "|||"
    "^^^"
    "~~~"
    "<<<"
    ">>>"
    "|>"
    "<|"
    ">>"
    "<<"
    "@"
    "::"
    "|"
    "->"
    "<-"
    ":>"
    ":?>"
    ":?"
    "!"
    ":="
    ".."
    ".." ]
  |> Set.ofList

let private isUnaryOperator (src: ISourceText) (operatorRange: range) =
  try
    let line = src.GetLineString(operatorRange.StartLine - 1)
    let beforeOperator = line.Substring(0, operatorRange.StartColumn).TrimEnd()
    if beforeOperator.Length > 0 then
      let lastChar = beforeOperator[beforeOperator.Length - 1]
      Char.IsLetterOrDigit lastChar || lastChar = ')' || lastChar = ']'
    else
      false
  with
    _ -> false

let private isCustomOperator (src: ISourceText) (funcRange: range) =
  let symbol = src.GetSubTextFromRange funcRange |> fun s -> s.Trim()
  if symbol.Length = 0 then
    false
  else
    (symbol |> Seq.forall (fun c -> not (Char.IsLetterOrDigit c) && c <> '_'))
    && not (Set.contains symbol generalOperator)

let private isOperator src = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(trivia = triv); range = range) ->
    triv
    |> List.exists (function
      | Some(IdentTrivia.OriginalNotation "=")
        -> false
      | Some(IdentTrivia.OriginalNotation _) -> true
      | _ -> false
    ) && isUnaryOperator src range
  | _ -> false

let private isEquality = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(trivia = trivias)) ->
    trivias
    |> List.exists (function
      | Some(IdentTrivia.OriginalNotation "=") -> true
      | _ -> false
    )
  | _ -> false

let rec private collectLastElementRange acc = function
  | SynExpr.App(argExpr = argExpr) ->
    match argExpr with
    | SynExpr.App _ -> collectLastElementRange acc argExpr
    | SynExpr.Tuple(exprs = exprs) -> (List.last exprs).Range
    | _ -> argExpr.Range
  | SynExpr.Tuple(exprs = exprs) -> (List.last exprs).Range
  | expr -> expr.Range

/// Retrieve the range of the element to the left of the operator from the given
/// source text. This is useful for analyzing expressions in the source code.
let private findLeftExprFromSource (src: ISourceText) (operatorRange: range) =
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
    _ -> None

let private shouldCheckFuncSpacing funcExpr (argExpr: SynExpr) =
  if argExpr.IsArrayOrListComputed then false
  else
    match funcExpr with
    | SynExpr.DotGet _
    | SynExpr.App(funcExpr = SynExpr.DotGet _) -> false
    | SynExpr.Ident _ -> true
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ first ])) ->
      first.idText.Length > 0 && not (Char.IsLower(first.idText[0]))
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = _ :: _ :: _)) -> false
    | SynExpr.App(funcExpr = SynExpr.Ident _)
    | SynExpr.App(funcExpr =
      SynExpr.LongIdent(longDotId = SynLongIdent(id = [ _ ]))) -> true
    | _ -> false

let private ensureInfixSpacing src funcRange (subArgRange: range) argRange =
  if (argRange: range).StartLine = (funcRange: range).StartLine
    && argRange.StartColumn <> funcRange.EndColumn + 1
  then Range.mkRange "" funcRange.End argRange.Start |> reportInfixError src
  else ()
  if subArgRange.StartLine = funcRange.StartLine
    && funcRange.StartColumn <> subArgRange.EndColumn + 1
  then Range.mkRange "" subArgRange.End funcRange.Start |> reportInfixError src
  else ()

let private ensureAddressOfSpacing src (exprRange: range) (opRange: range) =
  if opRange.StartLine = exprRange.StartLine
    && opRange.EndColumn <> exprRange.StartColumn
  then reportInfixError src opRange
  else ()

let private ensureFuncSpacing src (funcRange: range) (argRange: range) =
  if (argRange.StartColumn - funcRange.EndColumn <> 1
    && funcRange.StartLine = argRange.StartLine)
    && not (isUnaryOperator src funcRange)
    && not (isCustomOperator src funcRange)
  then
    Range.mkRange "" funcRange.End argRange.Start
    |> fun range -> reportWarn src range "Use single whitespace in func app"
  else ()

/// Validates spacing around (=) operators ensuring proper whitespace.
/// Checks for exact spacing requirements in assignment expressions.
let private checkAssignSpacing src (fRange: range) (subArgRange: range) aRange =
  if fRange.StartColumn <> subArgRange.EndColumn + 1
  then
    Range.mkRange "" subArgRange.End fRange.Start
    |> fun range -> reportWarn src range "Use single whitespace after '='"
  elif fRange.EndColumn <> (aRange: range).StartColumn - 1
  then
    Range.mkRange "" fRange.End aRange.Start
    |> fun range -> reportWarn src range "Use single whitespace before '='"
  else ()

/// Check proper spacing in infix applications.
/// Ensures a single space from the operator to each applied element
/// (e.g., "a + b", not "a+b").
let rec checkInfixSpacing src isInfix funcExpr (argExpr: SynExpr) =
  let processSubExpression subFuncExpr (subArgExpr: SynExpr) subIsInfix =
    if (isInfix || subIsInfix) && isOperator src subFuncExpr then
      ensureInfixSpacing src subFuncExpr.Range subArgExpr.Range argExpr.Range
    elif isEquality subFuncExpr then
      checkAssignSpacing src subFuncExpr.Range subArgExpr.Range argExpr.Range
    else
      ()
  match funcExpr with
  | SynExpr.App(isInfix = isInfixInner; funcExpr = subFuncExpr
                argExpr = subArgExpr) ->
    processSubExpression subFuncExpr subArgExpr isInfixInner
    match subArgExpr with
    | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
      checkInfixSpacing src isInfix funcExpr argExpr
    | _ -> ()
  | _ -> ()
  match argExpr with
  | SynExpr.App(isInfix = isInfixInner; funcExpr = subFuncExpr
                argExpr = subArgExpr) ->
    if isOperator src subFuncExpr then
      match findLeftExprFromSource src subFuncExpr.Range with
      | Some leafArgRange ->
        let subArgRange = collectLastElementRange subArgExpr.Range subArgExpr
        ensureInfixSpacing src subFuncExpr.Range leafArgRange subArgRange
      | None -> ()
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
  else
    ()
  match funcExpr with
  | SynExpr.App(isInfix = false; funcExpr = innerFunc; argExpr = innerArg) ->
    checkFuncSpacing src innerFunc innerArg
  | _ -> ()

let checkInfixOrFuncSpacing src isInfix funcExpr argExpr =
  if isInfix then checkInfixSpacing src isInfix funcExpr argExpr
  elif isOperator src funcExpr || shouldCheckFuncSpacing funcExpr argExpr
  then checkFuncSpacing src funcExpr argExpr
  else ()

let checkUnaryOperatorSpacing (src: ISourceText) (expr: SynExpr) =
  match expr with
  | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
    match funcExpr with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = identList)) ->
      match identList with
      | [ id ] when
        id.idText = "op_UnaryNegation" ||
        id.idText = "op_UnaryPlus" ||
        id.idText = "op_LogicalNot" ->
          if funcExpr.Range.EndLine = argExpr.Range.StartLine then
            let gap = argExpr.Range.StartColumn - funcExpr.Range.EndColumn
            if gap > 0 then
              Range.mkRange "" funcExpr.Range.End argExpr.Range.Start
              |> fun range ->
              reportWarn src range "Remove whitespace after unary operator"
            else
              ()
          else
            ()
      | _ -> ()
    | _ -> ()
  | _ -> ()

let checkLambdaArrowSpacing src pat body (trivia: SynExprLambdaTrivia) =
  if Option.isSome trivia.ArrowRange then
    if (pat: range).EndLine = trivia.ArrowRange.Value.StartLine
      && pat.EndColumn + 1 <> trivia.ArrowRange.Value.StartColumn
      && (body: range).StartColumn > pat.StartColumn
      && pat.EndColumn - pat.StartColumn > 1 then
      Range.mkRange "" pat.End trivia.ArrowRange.Value.Start
      |> fun range -> reportWarn src range "Use single whitespace before '->'"
    elif body.StartLine = trivia.ArrowRange.Value.StartLine
      && body.StartColumn - 1 <> trivia.ArrowRange.Value.EndColumn
      && body.StartColumn > trivia.ArrowRange.Value.EndColumn then
      Range.mkRange "" trivia.ArrowRange.Value.End body.Start
      |> fun range -> reportWarn src range "Use single whitespace after '->'"
    else
      ()
  else
    ()

let checkLambdaKeywordSpacing (src: ISourceText) lambdaRange argsRange =
  if (argsRange: range).StartLine = (lambdaRange: range).StartLine then
    let gap = Range.mkRange "" lambdaRange.Start argsRange.Start
    let gapStr = gap |> src.GetSubTextFromRange
    let keywordLen = if gapStr[0..3] = "func" then 8 else 3
    let handleException = gapStr[keywordLen..gapStr.Length - 1]
    if handleException.Length > 1 && handleException[1] <> ' ' then
      ()
    else
      gapStr
      |> String.filter (fun ch -> ch = ' ')
      |> fun ws ->
        if ws.Length <> 1 then
          Range.mkRange ""
            (Position.mkPos lambdaRange.StartLine
            (lambdaRange.StartColumn + keywordLen))
            argsRange.Start
          |> fun range ->
            reportWarn src range "Use single whitespace after Lambda"
        else
          ()
  else
    ()

let rec check src isInfix flag funcExpr (argExpr: SynExpr) =
  match funcExpr with
  | SynExpr.App(isInfix = subIsInfix; funcExpr = subFuncExpr
                argExpr = subArgExpr) ->
    checkInfixOrFuncSpacing src subIsInfix funcExpr argExpr
    check src subIsInfix flag subFuncExpr subArgExpr
  | SynExpr.LongIdent _ ->
    checkInfixOrFuncSpacing src isInfix funcExpr argExpr
  | SynExpr.Paren(expr = innerExpr) ->
    traverseParen src isInfix innerExpr
  | SynExpr.Ident _ ->
    match argExpr with
    | SynExpr.Paren(expr = innerExpr) -> traverseParen src isInfix innerExpr
    | _ -> ()
  | SynExpr.TypeApp _
  | SynExpr.DotGet _
  | SynExpr.DotLambda _ -> ()
  | expr -> warn $"[AppConvention] TODO: {expr}"
  match argExpr with
  | SynExpr.App(isInfix = isInfix; funcExpr = funcExpr; argExpr = argExpr) ->
    check src (isInfix || isOperator src funcExpr) flag funcExpr argExpr
  | SynExpr.Paren(expr = innerExpr) ->
    traverseParen src isInfix innerExpr
  | SynExpr.AddressOf _ ->
    checkInfixSpacing src true funcExpr argExpr
  | _ -> ()

and traverseParen src isInfix = function
  | SynExpr.App(flag = flag; funcExpr = subFuncExpr; argExpr = subArgExpr) ->
    check src isInfix flag subFuncExpr subArgExpr
  | SynExpr.Lambda(body = body) ->
    traverseParen src isInfix body
  | _ -> ()