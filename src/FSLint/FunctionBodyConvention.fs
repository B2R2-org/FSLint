module B2R2.FSLint.FunctionBodyConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let private findIdxRange fileName lineNumber startCol endColExclusive line =
  let rec loop pos acc =
    if pos <= endColExclusive - 3 then
      if (line: string).Substring(pos, 3) = "\"\"\"" then
        let tripleQuoteRange =
          Range.mkRange fileName (Position.mkPos lineNumber pos)
            (Position.mkPos lineNumber (pos + 3))
        loop (pos + 3) (tripleQuoteRange :: acc)
      else
        loop (pos + 1) acc
    else
      List.rev acc
  loop startCol []

let rec private unionRange acc ranges =
  match ranges with
  | startRange :: endRange :: rest ->
    unionRange (Range.unionRanges startRange endRange :: acc) rest
  | _ ->
    List.rev acc

let private getTripleQuoteRange (src: ISourceText) (range: range) =
  [ range.StartLine .. range.EndLine ]
  |> List.collect (fun lineNumber ->
    let line = src.GetLineString(lineNumber - 1)
    let startCol = if lineNumber = range.StartLine then range.StartColumn else 0
    let endCol =
      if lineNumber = range.EndLine then min range.EndColumn line.Length
      else line.Length

    if endCol - startCol < 3 then []
    else findIdxRange range.FileName lineNumber startCol endCol line
  )
  |> unionRange []
  |> fun ranges ->
    ranges
    |> List.map (fun r -> [ r.StartLine .. r.EndLine ])
    |> List.concat

let private isBlankLine (src: ISourceText) lineIdx =
  src.GetLineString(lineIdx - 1) |> String.IsNullOrWhiteSpace

let rec objExprRanges acc = function
  | SynExpr.ObjExpr(range = range) -> range :: acc
  | SynExpr.ComputationExpr(expr = expr)
  | SynExpr.TryWith(tryExpr = expr)
  | SynExpr.TryFinally(tryExpr = expr)
  | SynExpr.Do(expr = expr)
  | SynExpr.Paren(expr = expr)
  | SynExpr.Typed(expr = expr) -> objExprRanges acc expr
  | SynExpr.LetOrUse(bindings = bindings; body = body) ->
    bindings
    |> List.fold (fun acc (SynBinding(expr = b)) -> objExprRanges acc b) acc
    |> fun acc -> objExprRanges acc body
  | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
    objExprRanges (objExprRanges acc e1) e2
  | SynExpr.App(funcExpr = f; argExpr = a) ->
    objExprRanges (objExprRanges acc f) a
  | SynExpr.IfThenElse(ifExpr = iExpr; thenExpr = tExpr; elseExpr = eExpr) ->
    let acc = objExprRanges acc iExpr
    let acc = objExprRanges acc tExpr
    if Option.isSome eExpr then objExprRanges acc eExpr.Value else acc
  | _ -> acc

let private checkObjExprNewline src (objExprRanges: range list) =
  let findMultiline src acc lineIdx =
    if isBlankLine src lineIdx then
      if acc >= 1 then
        Range.mkRange objExprRanges[0].FileName
          (Position.mkPos (lineIdx - 1) 0) (Position.mkPos (lineIdx - 1) 1)
        |> fun range -> reportWarn src range "Use at most single blank line"
      else
        ()
      acc + 1
    else
      0
  for objExprRange: range in objExprRanges do
    [ objExprRange.StartLine .. objExprRange.EndLine ]
    |> List.fold (fun acc lineIdx -> findMultiline src acc lineIdx) 0
    |> ignore

let checkBinding src objRange (binding: SynBinding) =
  if isStrict then
    let range = binding.RangeOfBindingWithRhs
    let tripleQuote = getTripleQuoteRange src range
    [ range.StartLine .. range.EndLine ]
    |> List.filter (fun line ->
      List.contains line objRange |> not
      && List.contains line tripleQuote |> not)
    |> List.iter (fun lineIdx ->
      if isBlankLine src lineIdx then
        Range.mkRange range.FileName (Position.mkPos lineIdx 0)
          (Position.mkPos lineIdx 1)
        |> fun range -> reportWarn src range "Remove blank line in let scope"
      else
        ()
    )
  else
    ()

let private checkNested src (bindings: list<SynBinding>) =
  bindings
  |> List.pairwise
  |> List.choose (fun (prevBinding, nextBinding) ->
    let SynBinding(expr = prevExpr) = prevBinding
    let SynBinding(trivia = nextTrivia) = nextBinding
    if nextTrivia.LeadingKeyword.IsAnd then
      let prevEnd = prevExpr.Range.EndRange
      let andStart = nextTrivia.LeadingKeyword.Range.StartRange
      Some(prevEnd, andStart)
    else
      None)
  |> List.iter (fun (prev, next) ->
    if isStrict then
      if prev.StartLine + 1 = next.StartLine then
        (Position.mkPos (prev.StartLine + 1) 0,
         Position.mkPos (prev.StartLine + 1) 0)
        ||> Range.mkRange prev.FileName
        |> fun range -> reportWarn src range "Use single blank line"
      elif next.StartLine - prev.StartLine > 2 then
        (Position.mkPos (prev.StartLine + 1) 0,
         Position.mkPos (next.StartLine - 1) 0)
        ||> Range.mkRange prev.FileName
        |> fun range -> reportWarn src range "Use single blank line"
      else
        ()
    elif next.StartLine - prev.StartLine > 2 then
      (Position.mkPos (prev.StartLine + 1) 0,
       Position.mkPos (next.StartLine - 1) 0)
      ||> Range.mkRange prev.FileName
      |> fun range -> reportWarn src range "Use single blank line"
    else
      ()
  )

let checkletObjExpr src bindings =
  bindings
  |> List.fold (fun ac (SynBinding(expr = body)) -> objExprRanges ac body) []
  |> fun objExprRanges ->
    checkObjExprNewline src objExprRanges
    objExprRanges |> List.collect (fun r -> [ r.StartLine .. r.EndLine ])

/// Checks whether each binding within the given range in the source text
/// contains a newline within its scope. This helps enforce the convention
/// that function bodies should be multi-line if they exceed 80 columns.
let checkBindings src bindings =
  checkNested src bindings
  let objExprRange = checkletObjExpr src bindings
  List.iter (checkBinding src objExprRange) bindings