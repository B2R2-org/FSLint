module B2R2.FSLint.AssignmentConvention

open FSharp.Compiler.Syntax

/// Checks if there is a required space around '=' in assignment patterns.
/// Used to enforce spacing conventions in pattern assignments.
let checkNamePatPairs src (longIdents: list<NamePatPairField>) =
  longIdents
  |> List.iter (fun pair ->
    let NamePatPairField(fieldName = fN; equalsRange = eRange; pat = pat) = pair
    eRange |> Option.iter (fun range ->
      if fN.Range.EndColumn + 1 <> range.StartColumn
        || pat.Range.StartColumn <> range.EndColumn + 1
      then Diagnostics.reportWarn src range "Use single whitespace around '='"
      else ()
    )
  )