module B2R2.FSLint.FunctionCallConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

/// Checks spacing for curried function calls in the given source text.
/// Ensures proper formatting between arguments in curried functions.
let checkCurriedFunctionSpacing (src: ISourceText) (expr: SynExpr) =
  for line in expr.Range.StartLine - 1 .. expr.Range.EndLine - 1 do
    let lineString = src.GetLineString line
    try
      let bracketIdx = lineString.LastIndexOf '('
      let commaIdx = lineString.LastIndexOf '.'
      if bracketIdx <> -1 && commaIdx <> -1 && commaIdx < bracketIdx then
        let subString =
          lineString.Substring(lineString.LastIndexOf('.', bracketIdx - 1) + 1)
        let isPascalCase = Char.IsUpper subString[0]
        let bracketIdx =
          bracketIdx - lineString.LastIndexOf('.', bracketIdx - 1) - 1
        if isPascalCase && subString.Chars(bracketIdx - 1) = ' ' then
          reportError src expr.Range "No space between exprs"
        elif not isPascalCase && subString.Chars(bracketIdx - 1) <> ' ' then
          reportError src expr.Range "Need single space between expr"
        else ()
      else ()
    with | _ -> warn $"[checkCurriedFunction]TODO: {expr}"

let rec check (src: ISourceText) (expr: SynExpr) =
  match expr with
  | SynExpr.App (flag = flag; funcExpr = funcExpr; argExpr = argExpr) ->
    if funcExpr.IsApp then check src funcExpr
    else
      let argExprCondition =
        match argExpr with
        | SynExpr.Const (constant = constant) when constant.IsUnit -> true
        | SynExpr.Paren (expr = expr) when not expr.IsLambda -> true
        | _ -> false
      if flag = ExprAtomicFlag.Atomic || argExprCondition then
        checkCurriedFunctionSpacing src expr
      else ()
  | _ -> ()