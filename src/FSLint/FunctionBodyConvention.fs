module B2R2.FSLint.FunctionBodyConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let collectScopedLetLines bindings =
  bindings
  |> List.skip 1
  |> List.map (fun (binding: SynBinding) ->
    binding.RangeOfBindingWithoutRhs.StartLine - 1
  )

let private countTripleQuotes (line: string) =
  let rec count pos acc =
    if pos <= line.Length - 3 then
      if line.Substring(pos, 3) = "\"\"\"" then count (pos + 3) (acc + 1)
      else count (pos + 1) acc
    else
      acc
  count 0 0

let private isInsideStringLiteral (src: ISourceText) (range: range) lineIdx =
  let lines =
    [ range.StartLine - 1 .. lineIdx ] |> List.map (src.GetLineString)
  let totalTripleQuotes = lines |> List.sumBy countTripleQuotes
  totalTripleQuotes % 2 = 1

let checkNested (src: ISourceText) range lineNums =
  for lineNum in lineNums do
    if src.GetLineString(lineNum - 1) <> "" then
      reportError src range "Nested should be separated by exactly one space."
    else ()

let checkBlankLine (src: ISourceText) (range: range) lineNums =
  for lineIdx in range.StartLine .. range.EndLine - 1 do
    if List.contains (lineIdx + 1) lineNums then
      ()
    elif src.GetLineString lineIdx = "" then
      if isInsideStringLiteral src range lineIdx then ()
      else reportError src range "Remove empty line in function body."
    else
      ()

/// Checks whether each binding within the given range in the source text
/// contains a newline within its scope. This helps enforce the convention
/// that function bodies should be multi-line if they exceed 80 columns.
let checkBindings src range bindings =
  collectScopedLetLines bindings
  |> fun linenums ->
    checkNested src range linenums
    linenums
    |> checkBlankLine src range