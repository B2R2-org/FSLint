module B2R2.FSLint.FunctionCallConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let checkCurriedFunctionSpacing (src: ISourceText) (expr: SynExpr) =
  let lineString = src.GetLineString(expr.Range.EndLine - 1)
  let subString = lineString.Substring(lineString.LastIndexOf "." + 1)
  let isPascalCase = Char.IsUpper subString[0]
  let openingBracketIdx = subString.IndexOf '('
  if openingBracketIdx <> -1 then
    if isPascalCase && subString.Chars(openingBracketIdx - 1) = ' ' then
      reportError src expr.Range "No space between exprs"
    elif not isPascalCase && subString.Chars(openingBracketIdx - 1) <> ' ' then
      reportError src expr.Range "Need single space between expr"
    else ()
  else ()

let check (src: ISourceText) (expr: SynExpr) =
  match expr with
  | SynExpr.App (argExpr = argExpr) when not argExpr.IsArrayOrListComputed ->
    checkCurriedFunctionSpacing src expr
  | _ -> ()
