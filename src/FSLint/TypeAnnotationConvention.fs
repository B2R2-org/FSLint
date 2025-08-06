module B2R2.FSLint.TypeAnnotation

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let private reportTypeError src range =
  reportError src range "Type annotation does not follow the convention."

let private checkColonSpace src (patRange: range) typeLen (range: range) =
  if range.EndColumn <> patRange.EndColumn + typeLen + 2 then
    reportTypeError src range
  else
    let pat = src.GetSubTextFromRange range
    let colon = pat.Substring(patRange.EndColumn - patRange.StartColumn, 1)
    if colon <> ":" then reportTypeError src range else ()

let check src (pat: SynPat) range = function
  | SynType.LongIdent(SynLongIdent([ id ], _, _)) ->
    checkColonSpace src pat.Range id.idText.Length range
  | SynType.LongIdent(SynLongIdent([ id1; id2 ], _, _)) ->
    let typeLen = id1.idText.Length + id2.idText.Length + 1
    checkColonSpace src pat.Range typeLen range
  | SynType.App(_, _, _, _, _, _, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.Array(_, _, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.Var(_, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.Fun(_, _, typRange, _) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.HashConstraint(_, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | typ -> warn $"TODO: [Type Annotation] {typ}"