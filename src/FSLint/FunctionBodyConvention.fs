module B2R2.FSLint.FunctionBodyConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

/// Checks whether each binding within the given range in the source text
/// contains a newline within its scope. This helps enforce the convention
/// that function bodies should be multi-line if they exceed 80 columns.
let checkBindings (src: ISourceText) (range: range) bindings =
  let recurseIdxs =
    bindings
    |> List.skip 1
    |> List.map (fun (binding: SynBinding) ->
      binding.RangeOfBindingWithoutRhs.StartLine - 1
    )
  for recurseIdx in recurseIdxs do
    if src.GetLineString(recurseIdx - 1) <> "" then
      reportError src range "Nested should be separated by exactly one space."
    else ()
  for lineIdx in range.StartLine .. range.EndLine - 1 do
    if List.contains (lineIdx + 1) recurseIdxs then
      ()
    elif src.GetLineString lineIdx = "" then
      reportError src range "Remove empty line in function body."
    else ()

let rec check src = function
  | SynModuleDecl.NestedModule(decls = decls) ->
    for decl in decls do check src decl
  | SynModuleDecl.Let(_, bindings, range) ->
    checkBindings src range bindings
  | _ ->
    ()