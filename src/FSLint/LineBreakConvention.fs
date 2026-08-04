module B2R2.FSLint.LineBreakConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let [<Literal>] private Message = "Use consistent line breaks"

/// Returns true when the gap between two neighbouring items holds a line
/// break, i.e. the separator that joins them was broken across lines.
let private isBrokenGap (prev: range, next: range) =
  next.StartLine > prev.EndLine

/// Reports when the separators of a list are broken across lines only some of
/// the time. Every gap between neighbours must agree: either they all carry a
/// line break or none of them does.
let checkUniformPlacement src ranges =
  let gaps = ranges |> List.pairwise
  match gaps with
  | first :: _ when isStrict ->
    let firstIsBroken = isBrokenGap first
    gaps
    |> List.tryFind (fun gap -> isBrokenGap gap <> firstIsBroken)
    |> Option.iter (fun (_, next) -> reportWarn src next Message)
  | _ ->
    ()

/// Judges sibling bodies that hang off a keyword such as '->', 'then' or
/// 'else'. Each item pairs that keyword's range with the body's range.
let checkUniformBreak src (items: (range * range) list) =
  let isInline (keyword: range, body: range) =
    keyword.EndLine = body.StartLine
  if isStrict && List.length items > 1 then
    let firstIsInline = isInline (List.head items)
    items
    |> List.tryFind (fun item -> isInline item <> firstIsInline)
    |> Option.iter (fun (_, body) -> reportWarn src body Message)
  else
    ()

/// Checks a parameter list, covering both the tupled form `(a, b, c)` and the
/// curried form `a b c`. A tupled list is measured by its elements, because the
/// enclosing parentheses are a single pattern.
let checkParameters src (pats: SynPat list) =
  match pats with
  | [ SynPat.Paren(pat = SynPat.Tuple(elementPats = elements)) ] ->
    elements |> List.map (fun pat -> pat.Range) |> checkUniformPlacement src
  | _ ->
    pats |> List.map (fun pat -> pat.Range) |> checkUniformPlacement src
