module B2R2.FSLint.AssignmentConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let checkNamePatParis src (longIdents: list<Ident * option<range> * SynPat>) =
  longIdents
  |> List.iter (fun longIdent ->
    let ident, symbolRange, lastPat = longIdent
    if symbolRange.IsSome then
      if ident.idRange.EndColumn + 1 <> symbolRange.Value.StartColumn
        || lastPat.Range.StartColumn <> symbolRange.Value.EndColumn + 1
      then reportError src symbolRange.Value "Contains invalid whitespace"
      else ()
    else ()
  )