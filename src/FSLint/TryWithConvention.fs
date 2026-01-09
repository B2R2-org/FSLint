module B2R2.FSLint.TryWithConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let check (src: ISourceText) (clauses: SynMatchClause list) =
  if clauses.Length = 1 then
    let SynMatchClause(pat = pat; trivia = trivia) = clauses.Head
    match trivia.BarRange with
    | Some barR when not pat.IsOr ->
      let barLine = src.GetLineString(barR.StartLine - 1)
      let barText =
        barLine.Substring(barR.StartColumn, barR.EndColumn - barR.StartColumn)
      if barText.Trim() = "|" then
        (Position.mkPos barR.StartLine barR.StartColumn,
         Position.mkPos barR.StartLine barR.EndColumn)
        ||> Range.mkRange ""
        |> fun range ->
          reportWarn src range "Remove '|' for single exception case"
      else
        ()
    | _ ->
      ()
  else
    ()