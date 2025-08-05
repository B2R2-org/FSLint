module B2R2.FSLint.TypeAnnotation

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let [<Literal>] private ColonSpace = 2

let [<Literal>] private Dot = 1

let private reportTypeError src range =
  let message = "Type annotation does not follow the convention."
  reportError src range message

let private checkColonSpace src (patRange: range) typeLen (range: range) =
  if range.EndColumn <> patRange.EndColumn + ColonSpace + typeLen then
    reportTypeError src range
  else
    let pat = src.GetSubTextFromRange range
    let colon = pat.Substring(patRange.EndColumn - patRange.StartColumn, 1)
    if colon <> ":" then reportTypeError src range else ()

let check src pat typ range =
  let patRange =
    match pat with
    | SynPat.Named(range = patRange) -> patRange
    | SynPat.Wild(range = patRange) -> patRange
    | _ -> failwith $"Invalid Pattern: {pat}"
  match typ with
  | SynType.LongIdent(SynLongIdent([ id ], _, _)) ->
    checkColonSpace src patRange id.idText.Length range
  | SynType.LongIdent(SynLongIdent([ id1; id2 ], _, _)) ->
    let typeLen = id1.idText.Length + Dot + id2.idText.Length
    checkColonSpace src patRange typeLen range
  | SynType.App(_, _, _, _, _, _, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src patRange typeLen range
  | SynType.Array(_, _, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src patRange typeLen range
  | SynType.Var(_, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src patRange typeLen range
  | SynType.Fun(_, _, typRange, _) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src patRange typeLen range
  | SynType.HashConstraint(_, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src patRange typeLen range
  | _ -> warn $"TODO: [Type Annotation] {typ}"