module B2R2.FSLint.IfThenElseConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let checkKeywordSpacing src ifExpr thenExpr elseExpr trivia =
  let ifAdjustedRange =
    let ifRange = (ifExpr: SynExpr).Range.EndRange
    let thenRange = trivia.ThenKeyword.StartRange
    combineRangeWithComment ifRange thenRange true ifRange
  let thenAdjustedRange =
    let keyRange = trivia.ThenKeyword.EndRange
    let exprRange = (thenExpr: SynExpr).Range
    combineRangeWithComment keyRange exprRange.StartRange false exprRange
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

/// Collects every branch body of an if/elif/else chain, paired with whether
/// that body stayed on the same line as the keyword introducing it. An `elif`
/// is a nested `SynExpr.IfThenElse` flagged with `IsElif`, and only the last
/// link of the chain carries the real `else` keyword, so the whole chain has to
/// be walked before its layout can be judged as a whole.
let rec private collectBranches acc (thenExpr: SynExpr) elseExpr trivia =
  let acc =
    ((trivia: SynExprIfThenElseTrivia).ThenKeyword, thenExpr.Range) :: acc
  match elseExpr with
  | Some(SynExpr.IfThenElse(thenExpr = nextThen
                            elseExpr = nextElse
                            trivia = nextTrivia)) when nextTrivia.IsElif ->
    collectBranches acc nextThen nextElse nextTrivia
  | Some elseBody ->
    match trivia.ElseKeyword with
    | Some keyword ->
      (keyword, (elseBody: SynExpr).Range) :: acc
    | None ->
      acc
  | None ->
    acc

/// Returns true when the expression applies '&&' or '||' as an infix operator.
let private isBooleanConnective = function
  | SynExpr.App(funcExpr =
                  SynExpr.App(isInfix = true
                              funcExpr =
                                SynExpr.LongIdent(
                                  longDotId = SynLongIdent(id = [ op ])))) ->
    op.idText = "op_BooleanAnd" || op.idText = "op_BooleanOr"
  | _ ->
    false

/// Flattens a chain of '&&' and '||' into its operands, so that `a && b || c`
/// yields `[ a; b; c ]`.
let private flattenInfixChain expr =
  let rec loop acc e =
    match e with
    | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; argExpr = lhs)
                  argExpr = rhs) when isBooleanConnective e ->
      loop (rhs :: acc) lhs
    | _ ->
      e :: acc
  if isBooleanConnective expr then loop [] expr else []

/// Every branch of the chain must either stay inline or break onto its own
/// line; mixing the two is reported.
let private checkBranchLayout src thenExpr elseExpr trivia =
  if (trivia: SynExprIfThenElseTrivia).IsElif then
    ()
  else
    collectBranches [] thenExpr elseExpr trivia
    |> List.rev
    |> LineBreakConvention.checkUniformBreak src

/// Every operand of a boolean condition must either share one line or each sit
/// on its own line.
let private checkConditionLayout src ifExpr =
  flattenInfixChain ifExpr
  |> List.map (fun (operand: SynExpr) -> operand.Range)
  |> LineBreakConvention.checkUniformPlacement src

let check src ifExpr thenExpr (elseExpr: Option<SynExpr>) range trivia =
  if isStrict then
    checkBranchLayout src thenExpr elseExpr trivia
    checkConditionLayout src ifExpr
    match (trivia: SynExprIfThenElseTrivia).ElseKeyword with
    | Some _ ->
      checkKeywordSpacing src ifExpr thenExpr elseExpr.Value trivia
    | None ->
      let line =
        (src: ISourceText).GetLineString (thenExpr: SynExpr).Range.EndLine
      if line.TrimStart().StartsWith "elif" && Option.isSome elseExpr then
        checkKeywordSpacing src ifExpr thenExpr elseExpr.Value trivia
      elif line.TrimStart().StartsWith "else" && Option.isSome elseExpr then
        checkKeywordSpacing src ifExpr thenExpr elseExpr.Value trivia
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
  else
    ()