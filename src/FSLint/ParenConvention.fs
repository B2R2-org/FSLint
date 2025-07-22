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
let checkBracketSpacing (src: ISourceText) (range: range) =
  (* TODO: Cannot detect multi line *)
  if range.StartLine <> range.EndLine then ()
  else
    let openRange =
      (Position.mkPos range.StartLine range.StartColumn,
      Position.mkPos range.StartLine (range.StartColumn + 2))
      ||> Range.mkRange ""
    let endRange =
      (Position.mkPos range.StartLine (range.EndColumn - 2),
      Position.mkPos range.StartLine range.EndColumn)
      ||> Range.mkRange ""
    if (src.GetSubTextFromRange openRange).Contains " "
      || (src.GetSubTextFromRange endRange).Contains " "
    then reportError src range "Contains invalid whitespace"
    else ()

let rec check src = function
  | SynExpr.Paren(rightParenRange = rightParenRange; range = range) ->
    if rightParenRange.IsSome then
      checkBracketSpacing src range
    else ()
  | SynExpr.Const(SynConst.Unit, range) ->
    checkEmptySpacing src range
  | _ -> ()