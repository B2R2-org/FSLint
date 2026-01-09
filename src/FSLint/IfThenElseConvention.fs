module B2R2.FSLint.IfThenElseConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

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

let private calculateIfThenExprLength src (ifRange: range) ifExpr thenExpr =
  try
    let ifText = (src: ISourceText).GetSubTextFromRange((ifExpr: SynExpr).Range)
    let thenText = src.GetSubTextFromRange((thenExpr: SynExpr).Range)
    let indent = ifRange.StartColumn
    indent + 3 + ifText.Length + 6 + thenText.Length
  with _ ->
    Int32.MaxValue

let private calculateElseExprLength src (ifRange: range) (elseExpr: SynExpr) =
  try
    let elseText = (src: ISourceText).GetSubTextFromRange elseExpr.Range
    let indent = ifRange.StartColumn
    indent + 5 + elseText.Length
  with _ ->
    Int32.MaxValue

let private isCompactFormat src ifExpr (thenExpr: SynExpr) (elseExpr: SynExpr) =
  if (Range.unionRanges (ifExpr: SynExpr).Range elseExpr.Range
      |> (src: ISourceText).GetSubTextFromRange
      |> fun str -> str.ToCharArray() |> Array.contains '#') then
    true
  else
    let ifThenExprOneLine =
      (ifExpr: SynExpr).Range.StartLine = thenExpr.Range.EndLine
    let elseSeparated = thenExpr.Range.EndLine < elseExpr.Range.StartLine
    ifThenExprOneLine && elseSeparated

let private isSeparatedFormat ifExpr (thenExpr: SynExpr) (elseExpr: SynExpr) =
  let ifCondOneLine =
    (ifExpr: SynExpr).Range.StartLine = ifExpr.Range.EndLine
  let thenExprSeparated = ifExpr.Range.EndLine < thenExpr.Range.StartLine
  let elseSeparated = thenExpr.Range.EndLine < elseExpr.Range.StartLine
  ifCondOneLine && thenExprSeparated && elseSeparated

let check src ifExpr thenExpr (elseExpr: SynExpr option) (range: range) =
  match elseExpr with
  | None -> ()
  | Some elseExpr ->
    if (ifExpr: SynExpr).Range.StartLine = (thenExpr: SynExpr).Range.EndLine &&
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
      let ifThenExprFits = ifThenExprLength <= MaxLineLength
      let elseExprLength = calculateElseExprLength src range elseExpr
      let elseExprFits = elseExprLength <= MaxLineLength
      if ifThenExprFits && elseExprFits
        && not (isCompactFormat src ifExpr thenExpr elseExpr) then
          reportWarn src range "Use compact format (fits in 80 columns)"
      else
        ()