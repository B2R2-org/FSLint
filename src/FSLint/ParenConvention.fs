module B2R2.FSLint.ParenConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

/// Checks proper spacing in empty paren.
/// Ensures no space inside empty brackets (e.g., "()").
let checkEmptySpacing src (range: range) =
  if range.EndColumn - range.StartColumn <> 2 then
    Range.mkRange "" range.Start range.End
    |> fun range -> reportWarn src range "Remove whitespace in '()'"
  else
    ()

/// Avoid extraneous white space
let checkParenSpacing (src: ISourceText) (exprRange: range) (range: range) =
  if exprRange.StartLine = range.StartLine then
    if range.StartColumn + 1 <> exprRange.StartColumn then
      Range.mkRange range.FileName range.Start exprRange.Start
      |> fun range -> reportFrontParenInnerSpacing src range
    else
      ()
  elif exprRange.EndLine = range.EndLine then
    if exprRange.EndColumn + 1 <> range.EndColumn then
      Range.mkRange range.FileName exprRange.End range.End
      |> fun range -> reportBackParenInnerSpacing src range
    else
      ()
  else
    ()

let rec checkExpr src = function
  | SynExpr.Paren(expr = expr; range = range) ->
    checkParenSpacing src expr.Range range
  | SynExpr.Const(SynConst.Unit, range) ->
    checkEmptySpacing src range
  | _ -> ()

let rec checkPat src = function
  | SynPat.Paren(SynPat.Const(constant = SynConst.Unit), range) ->
    checkEmptySpacing src range
  | SynPat.Paren(pat, range) ->
    if range.StartColumn + 1 <> pat.Range.StartColumn then
      Range.mkRange "" range.Start pat.Range.Start
      |> reportFrontParenInnerSpacing src
    elif range.EndColumn - 1 <> pat.Range.EndColumn then
      Range.mkRange "" pat.Range.End range.End
      |> reportBackParenInnerSpacing src
    else ()
    checkPat src pat
  | SynPat.Tuple(elementPats = elementPats) ->
    List.iter (checkPat src) elementPats
  | _ ->
    ()
