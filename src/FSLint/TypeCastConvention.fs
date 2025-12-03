module B2R2.FSLint.TypeCastConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let private reportInfixError src range =
  reportError src range "There must be a space before and after the infix"

let collectCastSymbolRangeFromSrc (src: ISourceText) (expr: SynExpr)
  (targetType: SynType) =
  try
    let line = src.GetLineString(expr.Range.StartLine - 1)
    let searchStartColumn = expr.Range.EndColumn
    let searchEndColumn = targetType.Range.StartColumn
    let symbolStr =
      line.Substring(searchStartColumn, searchEndColumn - searchStartColumn)
      |> fun s -> s.Trim()
    let searchSubstring = line.Substring searchStartColumn
    let relativeSymbolStart = searchSubstring.IndexOf symbolStr
    if relativeSymbolStart = -1 then
      Range.range0
    else
      let absoluteSymbolStart = searchStartColumn + relativeSymbolStart
      let symbolEnd = absoluteSymbolStart + symbolStr.Length
      (Position.mkPos expr.Range.StartLine absoluteSymbolStart,
       Position.mkPos expr.Range.StartLine symbolEnd)
      ||> Range.mkRange ""
  with
  (* TODO: Cannot detect multi line *)
  | :? System.ArgumentOutOfRangeException -> Range.range0

/// Checks the spacing around the upcast operator (:>) in infix expressions.
let check src expr (targetType: SynType) =
  let symbolRange = collectCastSymbolRangeFromSrc src expr targetType
  if symbolRange.Equals Range.range0 then ()
  elif expr.Range.EndColumn + 1 <> symbolRange.StartColumn
    || targetType.Range.StartColumn - 1 <> symbolRange.EndColumn
  then reportInfixError src symbolRange
  else ()