module B2R2.FSLint.TypeAnnotation

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let [<Literal>] private ColonSpace = 2

let check pat (typ: SynType) (range: range) =
  let patRange =
    match pat with
    | SynPat.Named (range = patRange) -> patRange
    | _ -> failwith $"Invalid Pattern: {pat}"
  match typ with
  | SynType.LongIdent (SynLongIdent([id], _, _)) ->
    let idLen = id.idText.Length
    let eLine = range.EndLine
    if range.EndColumn <> patRange.EndColumn + ColonSpace + idLen then
      raiseWithError
        $"Type annotation does not follow naming convention(Line:{eLine})."
    else ()
  | _ -> warn $"TODO: {typ}"