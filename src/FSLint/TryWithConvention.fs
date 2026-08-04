module B2R2.FSLint.TryWithConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

/// The 'try' body and the handler body form one group: either both sit beside
/// what introduces them or both break onto a line of their own.
let checkLayout src (tryExpr: SynExpr) clauses trivia =
  match clauses with
  | [] ->
    ()
  | SynMatchClause(resultExpr = handler; trivia = clauseTrivia) :: _ ->
    match clauseTrivia.ArrowRange with
    | Some arrowRange ->
      [ (trivia: SynExprTryWithTrivia).TryKeyword, tryExpr.Range
        arrowRange, (handler: SynExpr).Range ]
      |> LineBreakConvention.checkUniformBreak src
    | None ->
      ()

/// 'try' and 'finally' pair up the same way 'try' and 'with' do.
let checkFinallyLayout src (tryExpr: SynExpr) (finallyExpr: SynExpr) trivia =
  [ (trivia: SynExprTryFinallyTrivia).TryKeyword, tryExpr.Range
    trivia.FinallyKeyword, finallyExpr.Range ]
  |> LineBreakConvention.checkUniformBreak src

let check (src: ISourceText) (clauses: SynMatchClause list) =
  if isStrict && clauses.Length = 1 then
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