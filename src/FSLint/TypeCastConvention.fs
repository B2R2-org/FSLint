module B2R2.FSLint.TypeCastConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

(* Because the AST does not provide the range of the symbol,
this check is performed only when the cast expression and the
target type are on the same line. *)
let check (src: ISourceText) (expr: SynExpr) (targetType: SynType) symbolSize =
  if isStrict
    && expr.Range.EndLine = targetType.Range.StartLine
    && targetType.Range.StartColumn - expr.Range.EndColumn - symbolSize <> 2
  then
    Range.mkRange expr.Range.FileName expr.Range.End targetType.Range.Start
    |> fun range -> reportWarn src range "Use single whitespace around symbol"
  else
    ()