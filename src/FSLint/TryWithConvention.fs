module B2R2.FSLint.TryWithConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let check (src: ISourceText) (clauses: SynMatchClause list) =
  if clauses.Length = 1 then
    let SynMatchClause(trivia = trivia) = clauses.Head
    match trivia.BarRange with
    | Some barR ->
      let barLine = src.GetLineString(barR.StartLine - 1)
      let barText =
        barLine.Substring(barR.StartColumn, barR.EndColumn - barR.StartColumn)
      if barText.Trim() = "|" then
        reportWarn src barR "Remove '|' for single exception case"
      else
        ()
    | None ->
      ()
  else
    ()