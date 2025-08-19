module B2R2.FSLint.ParenConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

/// Checks proper spacing in empty paren.
/// Ensures no space inside empty brackets (e.g., "()").
let checkEmptySpacing src (range: range) =
  if range.EndColumn - range.StartColumn <> 2 then
    reportError src range "Contains invalid whitespace"
  else
    ()

/// Checks proper spacing inside brackets for paren.
/// Ensures single space after opening and before closing brackets.
let checkBracketSpacing (src: ISourceText) (range: range) =
  let subStr = src.GetSubTextFromRange(range).Split '\n'
  if subStr.Length = 1 then
    if subStr[0].StartsWith "( " || subStr[0].EndsWith " )" then
      reportError src range "Contains invalid whitespace"
    else
      ()
  else
    if (Array.head subStr).StartsWith "( " ||
      ((Array.last subStr).EndsWith " )" &&
      (Array.last subStr).TrimStart() <> ")") then
      reportError src range "Contains invalid whitespace"
    else
      ()

let rec checkExpr src = function
  | SynExpr.Paren(expr = expr; range = range) ->
    if expr.Range.StartLine <> range.StartLine
      || expr.Range.EndLine <> range.EndLine
    then ()
    else checkBracketSpacing src range
  | SynExpr.Const(SynConst.Unit, range) ->
    checkEmptySpacing src range
  | _ -> ()

let rec checkPat src = function
  | SynPat.Paren(SynPat.Const(constant = SynConst.Unit), range) ->
    checkEmptySpacing src range
  | SynPat.Paren(pat, range) ->
    if range.StartColumn + 1 <> pat.Range.StartColumn
      || range.EndColumn - 1 <> pat.Range.EndColumn
    then reportError src range "Contains invalid whitespace"
    else ()
  | _ ->
    ()
