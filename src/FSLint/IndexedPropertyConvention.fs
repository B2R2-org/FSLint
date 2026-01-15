module B2R2.FSLint.IndexedPropertyConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let rec private collectBracketInfoInAppExpr = function
  | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
    if argExpr.IsArrayOrListComputed
    then argExpr :: collectBracketInfoInAppExpr funcExpr
    else collectBracketInfoInAppExpr argExpr
  | _ -> []

let checkBracketSpacing src (innerRange: range) (range: range) =
  if range.StartColumn + 1 <> innerRange.StartColumn then
    Range.mkRange "" range.Start innerRange.Start
    |> fun range -> reportWarn src range "Remove whitespace after '['"
  elif range.EndColumn - 1 <> innerRange.EndColumn then
    Range.mkRange "" innerRange.End range.End
    |> fun range -> reportWarn src range "Remove whitespace before ']'"
  else
    ()

let checkIndexRangeSpacing src = function
  | SynExpr.IndexRange(opm = opm; expr1 = expr1; expr2 = expr2) ->
    match expr1, expr2 with
    | Some e1, Some _ when e1.Range.EndColumn <> opm.StartColumn ->
      Range.mkRange "" e1.Range.End opm.Start
      |> fun range -> reportWarn src range "Remove whitespace before '..'"
    | Some _, Some e2 when e2.Range.StartColumn <> opm.EndColumn ->
      Range.mkRange "" opm.End e2.Range.Start
      |> fun range -> reportWarn src range "Remove whitespace after '..'"
    | Some e1, None when e1.Range.EndColumn <> opm.StartColumn ->
      Range.mkRange "" e1.Range.End opm.Start
      |> fun range -> reportWarn src range "Remove whitespace before '..'"
    | None, Some e2 when e2.Range.StartColumn <> opm.EndColumn ->
      Range.mkRange "" opm.End e2.Range.Start
      |> fun range -> reportWarn src range "Remove whitespace after '..'"
    | _ ->
      ()
  | _ ->
    ()

/// Checks that there is no space between the bracket/element for indexers.
/// Ensures no space between bracket and element in indexer expressions.
let check src expr =
  collectBracketInfoInAppExpr expr
  |> List.iter (function
    | SynExpr.ArrayOrListComputed(expr = innerExpr; range = range) ->
      checkBracketSpacing src innerExpr.Range range
      checkIndexRangeSpacing src innerExpr
    | _ ->
      ()
  )