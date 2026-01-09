module B2R2.FSLint.FunctionCallConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics
open Utils

let private getHeadMethodName (idents: Ident list) =
  idents
  |> List.tryHead
  |> Option.map (fun ident -> ident.idText, ident.idRange)

let private getMethodName = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = id)) ->
    id |> List.tryLast |> Option.map (fun ident -> ident.idText, ident.idRange)
  | SynExpr.DotGet(longDotId = SynLongIdent(id = id)) ->
    getHeadMethodName id
  | SynExpr.Ident ident -> Some(ident.idText, ident.idRange)
  | _ -> None

let private isSymbolOrPunctuation c =
  Char.IsSymbol c || Char.IsPunctuation c

let private checkSpacingOrNot (src: ISourceText) (range: range) =
  try
    let createThreeRangeAfterEndCol =
      Range.mkRange "" (Position.mkPos range.EndLine range.EndColumn)
        (Position.mkPos range.EndLine (range.EndColumn + 3))
    let str = src.GetSubTextFromRange createThreeRangeAfterEndCol
    str.Length >= 2 && str[0] = ' ' && not (isSymbolOrPunctuation str[1]) &&
    str = " wi" || str = " th" (* Heuristic: 'with' / 'then' detection *)
  with
    _ -> true

let private getLineAfterExpr (src: ISourceText) (expr: SynExpr) =
  let line = src.GetLineString(expr.Range.EndLine - 1)
  if expr.Range.EndColumn < line.Length then
    line.Substring(expr.Range.EndColumn).TrimStart()
  else
    ""

let private ensureMethodSpacing src flag funcExpr =
  match getMethodName funcExpr with
  | Some(methodName, methodRange) when methodName.Length > 0 ->
    let spaceRange =
      (methodRange.End,
       Position.mkPos methodRange.StartLine (methodRange.EndColumn + 1))
      ||> Range.mkRange ""
    if isPascalCase methodName && flag = ExprAtomicFlag.NonAtomic then
      reportPascalCaseError src spaceRange
    elif isPascalCase methodName |> not && flag = ExprAtomicFlag.Atomic then
      reportLowerCaseError src spaceRange
    else
      ()
  | _ -> ()

let checkTypeApp src expr (typeRange: range) argExpr =
  if not ((getLineAfterExpr src argExpr).StartsWith("(")) then
    match getMethodName expr, argExpr with
    | Some(name, _), SynExpr.Paren(range = range)
    | Some(name, _), SynExpr.Const(SynConst.Unit, range)
      when isPascalCase name ->
      if typeRange.EndColumn <> range.StartColumn then
        Range.mkRange "" typeRange.End range.Start
        |> reportPascalCaseError src
      else
        ()
    | _ -> ()

let checkIdent src (ident: Ident) argExpr =
  if not ((getLineAfterExpr src argExpr).StartsWith("(")) then
    let isIdentPascalCase = isPascalCase ident.idText
    if isIdentPascalCase
      && argExpr.Range.StartColumn <> ident.idRange.EndColumn
    then
      Range.mkRange "" ident.idRange.End argExpr.Range.Start
      |> reportPascalCaseError src
    elif argExpr.Range.StartColumn <> ident.idRange.EndColumn + 1
    then
      Range.mkRange "" ident.idRange.End argExpr.Range.Start
      |> reportLowerCaseError src
    else
      ()

let checkDotGet src expr id flag =
  match expr with
  | SynExpr.App(funcExpr = funcExpr
                argExpr = SynExpr.Const(SynConst.Unit, _)) ->
    getMethodName funcExpr
  | _ -> getMethodName expr
  |> fun str -> str |> Option.map fst
  |> fun prevMethodName ->
    match prevMethodName, getHeadMethodName id |> Option.map fst with
    | Some prevMethod, Some currentMethod
      when prevMethod.Length > 0 && currentMethod.Length > 0 ->
        let prevIsPascalCase = isPascalCase prevMethod
        let currentIsPascalCase = isPascalCase currentMethod
        let isNonAtomic = flag = ExprAtomicFlag.NonAtomic
        if not prevIsPascalCase && currentIsPascalCase && isNonAtomic then
          reportPascalCaseError src expr.Range
        elif prevIsPascalCase && not currentIsPascalCase && not isNonAtomic then
          reportLowerCaseError src expr.Range
        else
          ()
    | _ ->
      ()

let checkNewKeywordSpacing src = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ id ])),
    (argExpr: SynExpr)
    when id.idText = "new" && argExpr.IsParen
    && id.idRange.EndColumn <> argExpr.Range.StartColumn ->
    Range.mkRange "" id.idRange.End argExpr.Range.Start
    |> fun range -> reportWarn src range "Remove whitespace before '('"
  | _ ->
    ()

/// Checks spacing between method ident and paren based on naming convention.
/// Ensures that PascalCase have no space before paren
/// and lowerCase methods have a single space.
let rec checkMethodParenSpacing (src: ISourceText) (expr: SynExpr) =
  let checkByFlag flag funcExpr =
    ensureMethodSpacing src flag funcExpr
    checkMethodParenSpacing src funcExpr
  match expr with
  | SynExpr.App(flag = ExprAtomicFlag.NonAtomic
                funcExpr = funcExpr
                argExpr = argExpr) ->
    match funcExpr with
    | SynExpr.TypeApp(expr = expr; range = typeRange) ->
      checkTypeApp src expr typeRange argExpr
    | SynExpr.LongIdent(isOptional = false; longDotId = SynLongIdent(id = id))
      when id.Length <> 1 && argExpr.IsParen
      && argExpr.Range.StartLine = (List.last id).idRange.StartLine ->
      checkIdent src (List.last id) argExpr
    | SynExpr.Ident ident when argExpr.IsParen ->
      checkIdent src ident argExpr
    | _ ->
      ()
    match argExpr with
    | SynExpr.Paren(expr = parenExpr; range = range) ->
      if checkSpacingOrNot src range then
        ensureMethodSpacing src ExprAtomicFlag.NonAtomic funcExpr
      else
        ()
      checkMethodParenSpacing src funcExpr
      checkMethodParenSpacing src parenExpr
    | SynExpr.Const(SynConst.Unit, _) ->
      checkByFlag ExprAtomicFlag.NonAtomic funcExpr
    | _ ->
      ()
  | SynExpr.App(flag = flag
                funcExpr = funcExpr
                argExpr = SynExpr.Const(SynConst.Unit, _)) ->
    checkByFlag flag funcExpr
  | SynExpr.App(flag = flag
                funcExpr = funcExpr
                argExpr = SynExpr.Paren(expr = parenExpr; range = range)) ->
    if checkSpacingOrNot src range then checkByFlag flag funcExpr
    else checkMethodParenSpacing src funcExpr
    checkMethodParenSpacing src parenExpr
  | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
    checkNewKeywordSpacing src (funcExpr, argExpr)
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

let checkDotGetSpacing src (expr: SynExpr) (dotRange: range) longDotId =
  if expr.Range.EndLine = dotRange.StartLine
    && expr.Range.EndColumn <> dotRange.StartColumn then
    Range.mkRange "" expr.Range.End dotRange.Start
    |> fun range -> reportWarn src range "Remove whitespace before '.'"
  else
    ()
  match longDotId with
  | SynLongIdent(id = id) ->
    if dotRange.EndLine = id[0].idRange.StartLine
      && dotRange.EndColumn <> id[0].idRange.StartColumn then
      Range.mkRange "" dotRange.End id[0].idRange.Start
      |> fun range -> reportWarn src range "Remove whitespace after '.'"
    else ()