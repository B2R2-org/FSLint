module B2R2.FSLint.IndexedPropertyConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let rec collectBracketInfoInAppExpr = function
  | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
    if argExpr.IsArrayOrListComputed
    then argExpr :: collectBracketInfoInAppExpr funcExpr
    else collectBracketInfoInAppExpr argExpr
  | _ -> []

let checkBracketSpacing src (innerRange: range) (range: range) =
  if range.StartColumn + 1 <> innerRange.StartColumn
    || range.EndColumn - 1 <> innerRange.EndColumn then
    reportError src range "No space allowed in indexer"
  else ()

let checkIndexRangeSpacing src = function
  | SynExpr.IndexRange(opm = opm; expr1 = expr1; expr2 = expr2) ->
    match expr1, expr2 with
    | Some e1, Some e2 when
      e1.Range.EndColumn <> opm.StartColumn
      || e2.Range.StartColumn <> opm.EndColumn ->
      reportError src opm "No space allowed in indexer"
    | Some e1, None when e1.Range.EndColumn <> opm.StartColumn ->
      reportError src opm "No space allowed in indexer"
    | None, Some e2 when e2.Range.StartColumn <> opm.EndColumn ->
      reportError src opm "No space allowed in indexer"
    | _ -> ()
  | _ -> ()

/// Checks that there is no space between the bracket/element for indexers.
/// Ensures no space between bracket and element in indexer expressions.
let check src expr =
  collectBracketInfoInAppExpr expr
  |> List.iter (fun computed ->
    match computed with
    | SynExpr.ArrayOrListComputed(expr = innerExpr; range = range) ->
      checkBracketSpacing src innerExpr.Range range
      checkIndexRangeSpacing src innerExpr
    | _ -> ()
  )