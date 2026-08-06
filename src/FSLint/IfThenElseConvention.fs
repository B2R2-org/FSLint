module B2R2.FSLint.IfThenElseConvention

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

/// An `elif` and an `else if` are one and the same link, spelled in a word or
/// in two, and either way they carry the chain on rather than open a new one.
/// The first says so outright; the second is told by its `if` sitting on the
/// line the `else` above it ends. An `else` that hands its body a line of its
/// own is not that: what follows is a nested expression, judged on its own.
let private continuesChain elseKeyword (nested: SynExprIfThenElseTrivia) =
  nested.IsElif
  || match elseKeyword with
     | Some(keyword: range) -> keyword.EndLine = nested.IfKeyword.StartLine
     | None -> false

/// True when this `if` is the tail of an `else if`, and so belongs to a chain
/// already being judged from its head. Only the `else` can stand immediately
/// before an `if` on its line, so the text there tells it.
let private isElseIf (src: ISourceText) (ifKeyword: range) =
  let line = src.GetLineString(ifKeyword.StartLine - 1)
  if ifKeyword.StartColumn > line.Length then false
  else line.Substring(0, ifKeyword.StartColumn).TrimEnd().EndsWith "else"

/// Collects every branch body of an if/elif/else chain, paired with whether
/// that body stayed on the same line as the keyword introducing it. A link is
/// a nested `SynExpr.IfThenElse`, and only the last of them carries the real
/// `else` keyword, so the whole chain has to be walked before its layout can
/// be judged as a whole.
let rec private collectBranches acc (thenExpr: SynExpr) elseExpr trivia =
  let acc =
    ((trivia: SynExprIfThenElseTrivia).ThenKeyword, thenExpr.Range) :: acc
  match elseExpr with
  | Some(SynExpr.IfThenElse(thenExpr = nextThen
                            elseExpr = nextElse
                            trivia = nextTrivia)) when
      continuesChain trivia.ElseKeyword nextTrivia ->
    collectBranches acc nextThen nextElse nextTrivia
  | Some elseBody ->
    match trivia.ElseKeyword with
    | Some keyword -> (keyword, (elseBody: SynExpr).Range) :: acc
    | None -> acc
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

/// The chain is judged from the outside in, and the first question to settle
/// silences the rest.
///
/// It is held to the budget as a whole first: one that would close up onto a
/// single line has to be on a single line, so an 'elif' or 'else' left hanging
/// below is reported however neatly its own body sits beside it. A branch body
/// needing lines of its own puts that out of reach.
///
/// Then the links themselves must agree, all of them sharing a line or each on
/// its own. A chain too wide to close up may not close up the tail of itself
/// and leave the head above it broken: that is no more consistent for being
/// unreachable as a whole.
///
/// Only once the links sit right does each body answer for itself, staying
/// inline or breaking onto its own line as its siblings do. A condition broken
/// across lines says nothing about that: where the operands of a condition sit
/// and where a body sits beside its 'then' are separate questions, and a
/// condition too long for its line leaves the bodies free to answer as they
/// would have anyway.
let private checkBranchLayout src ifExpr thenExpr elseExpr range trivia =
  if (trivia: SynExprIfThenElseTrivia).IsElif || isElseIf src trivia.IfKeyword
  then
    ()
  else
    let branches = collectBranches [] thenExpr elseExpr trivia |> List.rev
    let links = List.map fst branches
    let fitsOnOneLine =
      branches
      |> List.forall (fun (_, body: range) -> body.StartLine = body.EndLine)
    if fitsOnOneLine && LineBreakConvention.checkClosesUp src range links then
      ()
    elif LineBreakConvention.checkGapAgreement src links then
      ()
    else
      LineBreakConvention.checkUniformBreak src branches

/// Every operand of a boolean condition must either share one line or each sit
/// on its own line.
let private checkConditionLayout src ifExpr =
  flattenInfixChain ifExpr
  |> List.map (fun (operand: SynExpr) -> operand.Range)
  |> LineBreakConvention.checkUniformPlacement src

/// An `elif` chain hands its `else` to the nested link that ends it, so every
/// link above that one has an else expression without an `else` keyword of its
/// own. Only a link holding neither is the one truly missing its else, and the
/// absent expression says so on its own, whatever line the chain is laid on.
let check src ifExpr thenExpr (elseExpr: Option<SynExpr>) range trivia =
  if isStrict then
    checkBranchLayout src ifExpr thenExpr elseExpr range trivia
    checkConditionLayout src ifExpr
    match elseExpr with
    | Some elseBody ->
      checkKeywordSpacing src ifExpr thenExpr elseBody trivia
    | None ->
      reportWarn src (trivia: SynExprIfThenElseTrivia).IfToThenRange
        "Add else expression"
  else
    ()