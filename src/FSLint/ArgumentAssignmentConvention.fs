module B2R2.FSLint.AssignmentConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

/// Checks if there is a required space around '=' in assignment patterns.
/// Used to enforce spacing conventions in pattern assignments.
let checkNamePatPairs src (longIdents: list<NamePatPairField>) =
  longIdents
  |> List.iter (fun pair ->
    let NamePatPairField(fieldName = fN; equalsRange = eRange; pat = pat) = pair
    eRange |> Option.iter (fun range ->
      if fN.Range.EndColumn + 1 <> range.StartColumn
        && fN.Range.StartLine = range.StartLine
      then
        Range.mkRange "" fN.Range.End range.Start
        |> fun range -> reportWarn src range "Use single whitespace before '='"
      elif pat.Range.StartColumn <> range.EndColumn + 1
        && pat.Range.StartLine = range.StartLine
      then
        Range.mkRange "" range.End pat.Range.Start
        |> fun range -> reportWarn src range "Use single whitespace after '='"
      else ()
    )
  )