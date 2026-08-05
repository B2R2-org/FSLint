module B2R2.FSLint.TryWithConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

/// The 'try' body and the handler form one group: either both sit beside what
/// introduces them or both break onto a line of their own. What the handler
/// hangs from depends on its shape. A barred handler is a list of cases
/// dangling under the 'with' keyword, so the leading '|' is what has to line up
/// with it; a bar-less `with pat -> body` fuses the keyword into the case, so
/// the '->' is the only place a break can land there. Either way the arrows of
/// the cases answer to each other alone, never to the 'try'.
///
/// A '|' never joins the 'with' it hangs from, however much room the line has
/// left, so a barred handler settles the whole group on the broken layout.
///
/// Before any of that, a bar-less 'try' is held to the budget as a whole: one
/// that would close up onto a single line has to be on a single line, so a
/// 'with' left hanging below is reported however neatly its own handler sits
/// beside it. A body needing lines of its own puts that out of reach.
let private closesUp src whole (bodies: range list) joints =
  bodies |> List.forall (fun body -> body.StartLine = body.EndLine)
  && LineBreakConvention.checkClosesUp src whole joints

let checkLayout src (tryExpr: SynExpr) clauses range trivia =
  match clauses with
  | [] ->
    ()
  | SynMatchClause(resultExpr = handler; trivia = clauseTrivia) :: _ ->
    match clauseTrivia.BarRange with
    | Some barRange ->
      [ (trivia: SynExprTryWithTrivia).TryKeyword, tryExpr.Range
        trivia.WithKeyword, barRange ]
      |> LineBreakConvention.checkUniformlyBroken src
    | None ->
      match clauseTrivia.ArrowRange with
      | Some arrowRange ->
        let handler = (handler: SynExpr).Range
        if closesUp src range [ tryExpr.Range; handler ]
             [ tryExpr.Range; trivia.WithKeyword; handler ] then
          ()
        else
          [ trivia.TryKeyword, tryExpr.Range
            arrowRange, handler ]
          |> LineBreakConvention.checkUniformBreak src
      | None ->
        ()

/// 'try' and 'finally' pair up the same way 'try' and 'with' do, the whole of
/// them held to the budget first.
let checkFinallyLayout src tryBody finallyBody whole trivia =
  let keyword = (trivia: SynExprTryFinallyTrivia).FinallyKeyword
  if closesUp src whole [ tryBody; finallyBody ]
       [ tryBody; keyword; finallyBody ] then
    ()
  else
    [ trivia.TryKeyword, tryBody
      keyword, finallyBody ]
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