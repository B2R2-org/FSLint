module B2R2.FSLint.TryWithConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let checkSingleCaseBar (src: ISourceText) (clauses: SynMatchClause list) =
  let SynMatchClause(trivia = trivia) = clauses.Head
  match trivia.BarRange with
  | Some barR ->
    let barLine = src.GetLineString(barR.StartLine - 1)
    let barText =
      barLine.Substring(barR.StartColumn, barR.EndColumn - barR.StartColumn)
    if barText.Trim() = "|" then
      reportError src barR
        "Remove unnecessary '|' for single exception case in try-with"
    else ()
  | None -> ()

let check (src: ISourceText) (clauses: SynMatchClause list) =
  checkSingleCaseBar src clauses