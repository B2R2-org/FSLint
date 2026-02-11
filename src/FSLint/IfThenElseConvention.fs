module B2R2.FSLint.IfThenElseConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let checkKeywordSpacing src ifExpr thenExpr elseExpr trivia codeTriv =
  let ifAdjustedRange =
    let ifRange = (ifExpr: SynExpr).Range.EndRange
    let thenRange = trivia.ThenKeyword.StartRange
    match findCommentsBetween codeTriv ifRange thenRange with
    | Some(CommentTrivia.LineComment range)
    | Some(CommentTrivia.BlockComment range) ->
      Range.unionRanges ifExpr.Range range
    | _ -> ifRange
  let thenAdjustedRange =
    let keyRange = trivia.ThenKeyword.EndRange
    let exprRange = (thenExpr: SynExpr).Range
    match findCommentsBetween codeTriv keyRange exprRange.StartRange with
    | Some(CommentTrivia.LineComment range)
    | Some(CommentTrivia.BlockComment range) ->
      Range.unionRanges range exprRange
    | _ -> exprRange
  if trivia.IfKeyword.EndLine = ifExpr.Range.StartLine
    && trivia.IfKeyword.EndColumn + 1 <> ifExpr.Range.StartColumn then
    Range.mkRange "" trivia.IfKeyword.End ifExpr.Range.Start
    |> fun range -> reportWarn src range "Use single whitespace after 'if'"
  elif trivia.ThenKeyword.EndLine = ifExpr.Range.EndLine
    && trivia.ThenKeyword.StartColumn - 1 <> ifAdjustedRange.EndColumn then
    Range.mkRange "" ifAdjustedRange.End trivia.ThenKeyword.Start
    |> fun range -> reportWarn src range "Use single whitespace before 'then'"
  elif trivia.ThenKeyword.EndLine = thenExpr.Range.StartLine
    && trivia.ThenKeyword.EndColumn + 1 <> thenAdjustedRange.StartColumn then
    Range.mkRange "" trivia.ThenKeyword.End thenAdjustedRange.Start
    |> fun range -> reportWarn src range "Use single whitespace after 'then'"
  elif Option.isSome trivia.ElseKeyword
    && trivia.ElseKeyword.Value.EndLine = (elseExpr: SynExpr).Range.StartLine
    && trivia.ElseKeyword.Value.EndColumn + 1 <> elseExpr.Range.StartColumn then
    Range.mkRange "" trivia.ElseKeyword.Value.End elseExpr.Range.Start
    |> fun range -> reportWarn src range "Use single space after 'else'"
  else
    ()

let check src ifExpr thenExpr (elseExpr: Option<SynExpr>) range trivia codeTri =
  match (trivia: SynExprIfThenElseTrivia).ElseKeyword with
  | Some _ ->
    checkKeywordSpacing src ifExpr thenExpr elseExpr.Value trivia codeTri
  | None ->
    let line =
      (src: ISourceText).GetLineString (thenExpr: SynExpr).Range.EndLine
    if line.TrimStart().StartsWith "elif" && Option.isSome elseExpr then
      checkKeywordSpacing src ifExpr thenExpr elseExpr.Value trivia codeTri
    elif line.TrimStart().StartsWith "else" && Option.isSome elseExpr then
      checkKeywordSpacing src ifExpr thenExpr elseExpr.Value trivia codeTri
    elif line.TrimStart().StartsWith "(*"
      || line.TrimStart().StartsWith "///" then
      Range.mkRange "" thenExpr.Range.Start (range: range).End
      |> (src: ISourceText).GetSubTextFromRange
      |> fun thenToEndStr ->
        if thenToEndStr.Contains "else " then ()
        elif thenToEndStr.Contains("else" + Environment.NewLine) then ()
        else reportWarn src trivia.IfToThenRange "Add else expression"
    else
      reportWarn src trivia.IfToThenRange "Add else expression"