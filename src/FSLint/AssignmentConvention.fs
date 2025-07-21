module B2R2.FSLint.AssignmentConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

/// Checks if there is a required space around '=' in assignment patterns.
/// Used to enforce spacing conventions in pattern assignments.
let checkNamePatParis src (longIdents: list<Ident * option<range> * SynPat>) =
  longIdents
  |> List.iter (fun longIdent ->
    let ident, symbolRange, lastPat = longIdent
    if symbolRange.IsSome then
      if ident.idRange.EndColumn + 1 <> symbolRange.Value.StartColumn
        || lastPat.Range.StartColumn <> symbolRange.Value.EndColumn + 1
      then reportError src symbolRange.Value "Need single space around '='"
      else ()
    else ()
  )