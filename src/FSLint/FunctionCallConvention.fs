module B2R2.FSLint.FunctionCallConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let isPascalCase (methodName: string) =
  methodName.Length > 0 && Char.IsUpper(methodName[0])

let private getHeadMethodName (idents: Ident list) =
  idents |> List.tryHead |> Option.map (fun ident -> ident.idText)

let private getMethodName = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = id)) ->
    id |> List.tryLast |> Option.map (fun ident -> ident.idText)
  | SynExpr.DotGet(longDotId = SynLongIdent(id = id)) ->
    getHeadMethodName id
  | SynExpr.Ident ident -> Some ident.idText
  | _ -> None

let reportPascalCaseError src range =
  reportError src range "No space between ident and paren"

let reportLowerCaseError src range =
  reportError src range "Need single space between ident and paren"

let reportFunctionCallError src range =
  reportError src range "Function call with Dot must be followed by paren."

let private isSymbolOrPunctuation c =
  System.Char.IsSymbol c || System.Char.IsPunctuation c

let private checkSpacingOrNot (src: ISourceText) (range: range) =
  try
    (Position.mkPos range.EndLine range.EndColumn,
     Position.mkPos range.EndLine (range.EndColumn + 3))
    ||> Range.mkRange ""
    |> src.GetSubTextFromRange
    |> function
      | str when str.Length >= 2 &&
                 str[0] = ' ' &&
                 not (isSymbolOrPunctuation str[1]) &&
                 str <> " wi" (* This is using in pattern matching *) &&
                 str <> " th"  ->
        false
      | _ -> true
  with | _ ->
    true

let getLineAfterExpr (src: ISourceText) (expr: SynExpr) =
  let line = src.GetLineString(expr.Range.EndLine - 1)
  if expr.Range.EndColumn < line.Length then
    line.Substring(expr.Range.EndColumn).TrimStart()
  else ""

let ensureMethodSpacing src flag funcExpr (expr: SynExpr) =
  match getMethodName funcExpr with
  | Some methodName when methodName.Length > 0 ->
    if isPascalCase methodName && flag = ExprAtomicFlag.NonAtomic then
      reportPascalCaseError src expr.Range
    elif isPascalCase methodName |> not && flag = ExprAtomicFlag.Atomic
    then reportLowerCaseError src expr.Range
    else ()
  | _ -> ()

let checkTypeApp src expr (typeRange: range) argExpr =
  if not ((getLineAfterExpr src argExpr).StartsWith("(")) then
    match getMethodName expr, argExpr with
    | Some name, SynExpr.Paren(range = range)
    | Some name, SynExpr.Const(SynConst.Unit, range) when isPascalCase name ->
      if typeRange.EndColumn <> range.StartColumn then
        reportPascalCaseError src range
      else ()
    | _ -> ()

let checkIdent src (ident: Ident) argExpr =
  if not ((getLineAfterExpr src argExpr).StartsWith("(")) then
    let isIdentPascalCase = isPascalCase ident.idText
    let expectedColumnForPascal = ident.idRange.EndColumn
    let expectedColumnForLower = ident.idRange.EndColumn + 1
    if isIdentPascalCase then
      if argExpr.Range.StartColumn <> expectedColumnForPascal then
        reportPascalCaseError src argExpr.Range
      else ()
    elif argExpr.Range.StartColumn <> expectedColumnForLower then
      reportLowerCaseError src argExpr.Range
    else ()

let checkDotGet src expr id flag =
  match expr with
  | SynExpr.App(funcExpr = funcExpr
                argExpr = SynExpr.Const(SynConst.Unit, _)) ->
    getMethodName funcExpr
  | _ -> getMethodName expr
  |> fun prevMethodName ->
    match prevMethodName, getHeadMethodName id with
    | Some prevMethod, Some currentMethod
      when prevMethod.Length > 0 && currentMethod.Length > 0 ->
        let prevIsPascalCase = isPascalCase prevMethod
        let currentIsPascalCase = isPascalCase currentMethod
        let isNonAtomic = flag = ExprAtomicFlag.NonAtomic
        if not prevIsPascalCase && currentIsPascalCase && isNonAtomic then
          reportPascalCaseError src expr.Range
        elif prevIsPascalCase && not currentIsPascalCase && not isNonAtomic then
          reportLowerCaseError src expr.Range
        else ()
    | _ -> ()

/// Checks spacing between method ident and paren based on naming convention.
/// Ensures that PascalCase have no space before paren
/// and lowerCase methods have a single space.
let rec checkMethodParenSpacing (src: ISourceText) (expr: SynExpr) =
  match expr with
  | SynExpr.App(flag = ExprAtomicFlag.NonAtomic
                funcExpr = funcExpr
                argExpr = argExpr) ->
    match funcExpr with
    | SynExpr.TypeApp(expr = expr; range = typeRange) ->
      checkTypeApp src expr typeRange argExpr
    | SynExpr.Ident ident when argExpr.IsParen -> checkIdent src ident argExpr
    | _ -> ()
    match argExpr with
    | SynExpr.Paren(range = range) ->
      if checkSpacingOrNot src range then
        ensureMethodSpacing src ExprAtomicFlag.NonAtomic funcExpr expr
      else ()
      checkMethodParenSpacing src funcExpr
    | SynExpr.Const(SynConst.Unit, _) ->
      ensureMethodSpacing src ExprAtomicFlag.NonAtomic funcExpr expr
      checkMethodParenSpacing src funcExpr
    | _ ->
      ()
  | SynExpr.App(flag = flag
                funcExpr = funcExpr
                argExpr = SynExpr.Const(SynConst.Unit, _)) ->
    ensureMethodSpacing src flag funcExpr expr
    checkMethodParenSpacing src funcExpr
  | SynExpr.App(flag = flag
                funcExpr = funcExpr
                argExpr = SynExpr.Paren(expr = parenExpr; range = range)) ->
    if checkSpacingOrNot src range then
      ensureMethodSpacing src flag funcExpr expr
      checkMethodParenSpacing src funcExpr
    checkMethodParenSpacing src parenExpr
  | SynExpr.DotGet(expr = expr; longDotId = SynLongIdent(id = id)) ->
    match expr with
    | SynExpr.App(flag = flag; argExpr = SynExpr.Const(SynConst.Unit, _)) ->
      checkDotGet src expr id flag
    | _ ->
      ()
    checkMethodParenSpacing src expr
  | SynExpr.Paren(expr = innerExpr) ->
    checkMethodParenSpacing src innerExpr
  | SynExpr.Tuple(exprs = exprs) ->
    exprs |> List.iter (checkMethodParenSpacing src)
  | _ ->
    ()