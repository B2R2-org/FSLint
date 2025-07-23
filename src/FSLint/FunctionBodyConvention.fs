module B2R2.FSLint.FunctionBodyConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

/// Checks if there is incorrect spacing in constructor calls.
let checkEmptyNewLine (src: ISourceText) (range: range) bindings =
  let recurseIdxs =
    bindings
    |> List.skip 1
    |> List.map (fun (binding: SynBinding) ->
      binding.RangeOfBindingWithoutRhs.StartLine - 1
    )
  for recurseIdx in recurseIdxs do
    if src.GetLineString(recurseIdx - 1) <> "" then
      reportError src range "Missing newline in outer binding."
    else ()
  for lineIdx in range.StartLine .. range.EndLine - 1 do
    if List.contains (lineIdx + 1) recurseIdxs then ()
    elif src.GetLineString lineIdx = "" then
      reportError src range "Remove empty line in function body."
    else ()
