module B2R2.FSLint.ParenConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

/// Checks proper spacing in empty paren.
/// Ensures no space inside empty brackets (e.g., "()").
let checkEmptySpacing src (range: range) =
  if range.EndColumn - range.StartColumn <> 2 then
    reportError src range "Contains invalid whitespace"
  else ()

/// Checks proper spacing inside brackets for paren.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing (src: ISourceText) (exprRange: range) (range: range) =
  if exprRange.StartLine <> range.StartLine
    || exprRange.EndLine <> range.EndLine
  then
    ()
  else
    (src.GetSubTextFromRange range).Split '\n'
    |> fun subStr ->
      if subStr.Length = 1 then
        if subStr[0].StartsWith "( " || subStr[0].EndsWith " )" then
          reportError src range "Contains invalid whitespace"
        else ()
      else
        if (Array.head subStr).StartsWith "( "
          || ((Array.last subStr).EndsWith " )"
          && (Array.last subStr).TrimStart() <> ")") then
          reportError src range "Contains invalid whitespace"
        else ()

let rec check src = function
  | SynExpr.Paren(expr = expr; range = range) ->
    checkBracketSpacing src expr.Range range
  | SynExpr.Const(SynConst.Unit, range) ->
    checkEmptySpacing src range
  | _ -> ()