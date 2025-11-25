module B2R2.FSLint.IfThenElseConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let rec private containsMatchOrBar (expr: SynExpr) =
  match expr with
  | SynExpr.Match _ | SynExpr.MatchBang _ | SynExpr.MatchLambda _ -> true
  | SynExpr.Paren(expr = inner) -> containsMatchOrBar inner
  | SynExpr.App(funcExpr = func; argExpr = arg) ->
    containsMatchOrBar func || containsMatchOrBar arg
  | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
    containsMatchOrBar e1 || containsMatchOrBar e2
  | SynExpr.IfThenElse(ifExpr = ie; thenExpr = te; elseExpr = ee) ->
    containsMatchOrBar ie || containsMatchOrBar te ||
    Option.map containsMatchOrBar ee |> Option.defaultValue false
  | _ -> false

let private isMultiLineExpr (expr: SynExpr) =
  expr.Range.StartLine <> expr.Range.EndLine

let private calculateIfThenExprLength
 (src: ISourceText) (ifRange: range) (ifExpr: SynExpr) (thenExpr: SynExpr) =
  try
    let ifText = src.GetSubTextFromRange(ifExpr.Range)
    let thenText = src.GetSubTextFromRange(thenExpr.Range)
    let indent = ifRange.StartColumn
    indent + 3 + ifText.Length + 6 + thenText.Length
  with
    _ -> System.Int32.MaxValue

let private calculateElseExprLength
 (src: ISourceText) (ifRange: range) (elseExpr: SynExpr) =
  try
    let elseText = src.GetSubTextFromRange(elseExpr.Range)
    let indent = ifRange.StartColumn
    indent + 5 + elseText.Length
  with
    _ -> System.Int32.MaxValue

let private isCompactFormat
 (ifExpr: SynExpr) (thenExpr: SynExpr) (elseExpr: SynExpr) =
  let ifThenExprOneLine = ifExpr.Range.StartLine = thenExpr.Range.EndLine
  let elseSeparated = thenExpr.Range.EndLine < elseExpr.Range.StartLine
  ifThenExprOneLine && elseSeparated

let private isSeparatedFormat
 (ifExpr: SynExpr) (thenExpr: SynExpr) (elseExpr: SynExpr) =
  let ifCondOneLine = ifExpr.Range.StartLine = ifExpr.Range.EndLine
  let thenExprSeparated = ifExpr.Range.EndLine < thenExpr.Range.StartLine
  let elseSeparated = thenExpr.Range.EndLine < elseExpr.Range.StartLine
  ifCondOneLine && thenExprSeparated && elseSeparated

let check
 (src: ISourceText) (ifExpr: SynExpr)
  (thenExpr: SynExpr) (elseExpr: SynExpr option) (range: range) =
  match elseExpr with
  | None -> ()
  | Some elseExpr ->
    if ifExpr.Range.StartLine = thenExpr.Range.EndLine &&
       thenExpr.Range.EndLine = elseExpr.Range.EndLine then
      ()
    elif isMultiLineExpr ifExpr ||
         isMultiLineExpr thenExpr ||
         isMultiLineExpr elseExpr ||
         containsMatchOrBar ifExpr ||
         containsMatchOrBar thenExpr ||
         containsMatchOrBar elseExpr then
      ()
    else
      let ifThenExprLength =
        calculateIfThenExprLength src range ifExpr thenExpr
      let ifThenExprFits = ifThenExprLength <= Utils.MaxLineLength
      let elseExprLength = calculateElseExprLength src range elseExpr
      let elseExprFits = elseExprLength <= Utils.MaxLineLength
      if ifThenExprFits && elseExprFits then
        if not (isCompactFormat ifExpr thenExpr elseExpr) then
          reportError src range
            ("Both 'if-then-expr' and 'else-expr' fit in 80 " +
             "columns")
      elif not (isSeparatedFormat ifExpr thenExpr elseExpr) then
        reportError src range "At least one part exceeds 80 columns"