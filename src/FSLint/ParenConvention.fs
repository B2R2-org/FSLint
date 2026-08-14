module B2R2.FSLint.ParenConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

/// Checks proper spacing in empty paren.
/// Ensures no space inside empty brackets (e.g., "()").
let checkEmptySpacing src (range: range) =
  if range.EndColumn - range.StartColumn <> 2 then
    Range.mkRange "" range.Start range.End
    |> fun range -> reportWarn src range "Remove whitespace in '()'"
  else
    ()

/// Avoid extraneous white space.
///
/// Each parenthesis answers for the side it stands on, and a side says nothing
/// about a line it is not on: an expression opened below its '(' has no gap
/// there to judge, and one closed above its ')' none either. The two are asked
/// in turn rather than one instead of the other, so a fence spaced wrongly at
/// the end is still named when it opened correctly.
///
/// What stands nearest the parenthesis is read out of the text rather than
/// taken from the expression's range, because a comment sits inside the fence
/// without belonging to the expression. `(count &&& 0b1111L (* COUNT *))` is
/// spaced exactly as it should be, though the expression ends a dozen
/// characters short of the ')'.
let checkParenSpacing (src: ISourceText) (exprRange: range) (range: range) =
  let textBetween start finish =
    Range.mkRange range.FileName start finish |> src.GetSubTextFromRange
  let leading =
    if exprRange.StartLine <> range.StartLine then
      0
    else
      let inside = (textBetween range.Start exprRange.Start).Substring 1
      inside.Length - inside.TrimStart().Length
  let trailing =
    if exprRange.EndLine <> range.EndLine then
      0
    else
      let text = textBetween exprRange.End range.End
      let inside = text.Substring(0, text.Length - 1)
      inside.Length - inside.TrimEnd().Length
  if leading > 0 then
    Position.mkPos range.StartLine (range.StartColumn + 1 + leading)
    |> Range.mkRange range.FileName range.Start
    |> fun range -> reportFrontParenInnerSpacing src range
  elif trailing > 0 then
    Position.mkPos range.EndLine (range.EndColumn - 1 - trailing)
    |> fun start -> Range.mkRange range.FileName start range.End
    |> fun range -> reportBackParenInnerSpacing src range
  else
    ()

/// The fence of a struct tuple, which the syntax tree does not hand out on its
/// own: `struct (a, b)` is one tuple node whose range takes in the keyword and
/// the parentheses alike, with no `SynExpr.Paren` to be asked about them. The
/// opening parenthesis is read out of the text and the closing one ends the
/// range, so the fence is judged as any other is.
let structFence (src: ISourceText) (range: range) =
  let line = src.GetLineString(range.StartLine - 1)
  let opening = line.IndexOf('(', range.StartColumn)
  if opening < 0 then
    None
  else
    Position.mkPos range.StartLine opening
    |> fun start -> Some(Range.mkRange range.FileName start range.End)

/// Judges the fence a struct tuple carries, given the tuple's whole range and
/// the ranges of its elements. A keyword spelled without one, should the parser
/// ever hand such a thing over, has no fence to answer for and is left alone.
let checkStructSpacing src (range: range) (elements: range list) =
  match structFence src range, elements with
  | Some fence, _ :: _ ->
    List.reduce Range.unionRanges elements
    |> fun span -> checkParenSpacing src span fence
  | _ ->
    ()

let rec checkExpr src = function
  | SynExpr.Paren(expr = SynExpr.TraitCall _) ->
    ()
  | SynExpr.Paren(expr = expr; range = range) ->
    checkParenSpacing src expr.Range range
  | SynExpr.Const(SynConst.Unit, range) ->
    checkEmptySpacing src range
  | _ ->
    ()

let rec checkPat src = function
  | SynPat.Paren(SynPat.Const(constant = SynConst.Unit), range) ->
    checkEmptySpacing src range
  | SynPat.Paren(pat, range) ->
    if range.StartColumn + 1 <> pat.Range.StartColumn then
      Range.mkRange "" range.Start pat.Range.Start
      |> reportFrontParenInnerSpacing src
    elif range.EndColumn - 1 <> pat.Range.EndColumn then
      Range.mkRange "" pat.Range.End range.End
      |> reportBackParenInnerSpacing src
    else
      ()
    checkPat src pat
  | SynPat.Tuple(isStruct = true; elementPats = elementPats; range = range) ->
    elementPats
    |> List.map (fun (pat: SynPat) -> pat.Range)
    |> checkStructSpacing src range
    List.iter (checkPat src) elementPats
  | SynPat.Tuple(elementPats = elementPats) ->
    List.iter (checkPat src) elementPats
  | _ ->
    ()
