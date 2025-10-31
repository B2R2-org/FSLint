module B2R2.FSLint.IfThenElseConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

[<Literal>]
let MaxLineLength = 80

let safeGetSubText (src: ISourceText) (range: range) =
  try
    if range.StartLine >= 1 && range.EndLine <= src.GetLineCount() then
      src.GetSubTextFromRange(range)
    else
      ""
  with
  | _ -> ""

let rec containsMatchOrBar (expr: SynExpr) =
  match expr with
  | SynExpr.Match _ | SynExpr.MatchBang _ | SynExpr.MatchLambda _ -> true
  | SynExpr.Paren(expr = inner) -> containsMatchOrBar inner
  | SynExpr.App(funcExpr = func; argExpr = arg) ->
    containsMatchOrBar func || containsMatchOrBar arg
  | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
    containsMatchOrBar e1 || containsMatchOrBar e2
  | SynExpr.IfThenElse(ifExpr = ie; thenExpr = te; elseExpr = ee) ->
    containsMatchOrBar ie || containsMatchOrBar te ||
    (
    match ee with
    | Some e -> containsMatchOrBar e
    | None -> false
    )
  | _ -> false

let isMultiLineExpr (expr: SynExpr) =
  expr.Range.StartLine <> expr.Range.EndLine

let calculateIfThenExprLength
 (src: ISourceText) (ifRange: range) (ifExpr: SynExpr) (thenExpr: SynExpr) =
  try
    let ifText = safeGetSubText src ifExpr.Range
    let thenText = safeGetSubText src thenExpr.Range
    if ifText = "" || thenText = "" then
      System.Int32.MaxValue
    else
      let indent = ifRange.StartColumn
      indent + 3 + ifText.Length + 6 + thenText.Length
  with
  | _ -> System.Int32.MaxValue

let calculateElseExprLength
 (src: ISourceText) (ifRange: range) (elseExpr: SynExpr) =
  try
    let elseText = safeGetSubText src elseExpr.Range
    if elseText = "" then
      System.Int32.MaxValue
    else
      let indent = ifRange.StartColumn
      indent + 5 + elseText.Length
  with
  | _ -> System.Int32.MaxValue

let isCompactFormat
 (ifExpr: SynExpr) (thenExpr: SynExpr) (elseExpr: SynExpr) =
  let ifThenExprOneLine = ifExpr.Range.StartLine = thenExpr.Range.EndLine
  let elseSeparated = thenExpr.Range.EndLine < elseExpr.Range.StartLine
  ifThenExprOneLine && elseSeparated

let isSeparatedFormat
 (ifExpr: SynExpr) (thenExpr: SynExpr) (elseExpr: SynExpr) =
  let ifCondOneLine = ifExpr.Range.StartLine = ifExpr.Range.EndLine
  let thenExprSeparated = ifExpr.Range.EndLine < thenExpr.Range.StartLine
  let elseSeparated = thenExpr.Range.EndLine < elseExpr.Range.StartLine
  ifCondOneLine && thenExprSeparated && elseSeparated

let hasMultiLineCondition (ifExpr: SynExpr) =
  ifExpr.Range.StartLine <> ifExpr.Range.EndLine

let isAllOnSameLine
 (ifExpr: SynExpr) (thenExpr: SynExpr) (elseExpr: SynExpr) =
  ifExpr.Range.StartLine = thenExpr.Range.EndLine &&
  thenExpr.Range.EndLine = elseExpr.Range.EndLine

let check
 (src: ISourceText) (ifExpr: SynExpr)
  (thenExpr: SynExpr) (elseExpr: SynExpr option) (range: range) =
  match elseExpr with
  | None -> ()
  | Some elseExpr ->
    if isAllOnSameLine ifExpr thenExpr elseExpr then
      ()
    else
      if hasMultiLineCondition ifExpr then
        ()
      else
        if isMultiLineExpr thenExpr || isMultiLineExpr elseExpr then
          ()
        else
          let hasMatchInCond = containsMatchOrBar ifExpr
          let hasMatchInThen = containsMatchOrBar thenExpr
          let hasMatchInElse = containsMatchOrBar elseExpr
          if hasMatchInCond || hasMatchInThen || hasMatchInElse then
            ()
          else
            let ifThenExprLength =
             calculateIfThenExprLength src range ifExpr thenExpr
            let ifThenExprFits = ifThenExprLength <= MaxLineLength
            let elseExprLength = calculateElseExprLength src range elseExpr
            let elseExprFits = elseExprLength <= MaxLineLength
            if ifThenExprFits && elseExprFits then
              let isCorrect = isCompactFormat ifExpr thenExpr elseExpr
              if not isCorrect then
                reportError src range
                  "Both 'if-then-expr' and 'else-expr' fit in 80 columns"
              else
                ()
            else
              let isCorrect = isSeparatedFormat ifExpr thenExpr elseExpr
              if not isCorrect then
                reportError src range
                  "At least one part exceeds 80 columns"
              else
                ()