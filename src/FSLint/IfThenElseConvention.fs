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
///
/// Both sides are walked, not the left alone. `&&` binds tighter than `||`, so
/// `a && b || c && d` is a `||` holding two `&&` pairs, and following only the
/// left spine would leave `c && d` standing as one operand with its own `&&`
/// hidden inside it. The reader sees four things separated by operators, and
/// the chain has to be read the same way.
let private flattenInfixChain expr =
  let rec loop acc e =
    match e with
    | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; argExpr = lhs)
                  argExpr = rhs) when isBooleanConnective e ->
      loop (loop acc rhs) lhs
    | _ ->
      e :: acc
  if isBooleanConnective expr then loop [] expr else []

/// True when a link of the chain carries another `if` as its body, bare of
/// parentheses. Such a chain can never be closed up: brought onto one line,
/// the inner `if` takes the `else` meant for the outer one and F# refuses to
/// read the result at all. Parenthesised, the inner `if` is an operand like
/// any other and the chain closes up as it would have.
let rec private carriesBareIf (thenExpr: SynExpr) elseExpr trivia =
  let elseKeyword = (trivia: SynExprIfThenElseTrivia).ElseKeyword
  match thenExpr with
  | SynExpr.IfThenElse _ ->
    true
  | _ ->
    match elseExpr with
    | Some(SynExpr.IfThenElse(thenExpr = nextThen
                              elseExpr = nextElse
                              trivia = nextTrivia)) when
        continuesChain elseKeyword nextTrivia ->
      carriesBareIf nextThen nextElse nextTrivia
    | _ ->
      false

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
    let bare = carriesBareIf thenExpr elseExpr trivia
    let fitsOnOneLine =
      branches
      |> List.forall (fun (_, body: range) -> body.StartLine = body.EndLine)
    let closesUp =
      not bare
      && fitsOnOneLine
      && LineBreakConvention.checkClosesUp src range
    if closesUp then ()
    elif LineBreakConvention.checkGapAgreement src links then ()
    elif bare then LineBreakConvention.checkUniformlyBroken src branches
    else LineBreakConvention.checkUniformBreak src branches

/// Every operand of a boolean condition must either share one line or each sit
/// on its own line.
///
/// Parentheses fence off a group of their own. To the chain outside them the
/// whole parenthesised operand counts as one, however many lines it runs to,
/// and what stands inside answers separately on the same terms. So a group
/// held on one line beside a broken outer chain is in order, while operands
/// disagreeing inside the parentheses are not, whatever the outer chain does.
/// A condition too wide for its line is not broken across several either: a
/// test spread over a paragraph stops reading as a test. What it wants is a
/// name, so that the `if` can say what it asks in one line again.
/// A condition too long for one line breaks at every one of its operators,
/// and the chain reads down the page well enough for that.
///
/// A parenthesised group is another matter. It is not a parameter list, and
/// once it needs a line break of its own the reader has to hold an unnamed
/// sub-condition in mind across it. What it wants then is a name: bound to a
/// let, it goes back to standing on one line inside the chain. So a group is
/// held to its own line, and asked for a name once it cannot keep to it.
let rec private checkConditionLayout src expr =
  match expr with
  | SynExpr.Paren(expr = inner; range = fence) ->
    if LineBreakConvention.closesUpWithin src fence then
      checkConditionLayout src inner
    else
      reportBindToLet src fence
  | _ ->
    let operands = flattenInfixChain expr
    operands
    |> List.map (fun (operand: SynExpr) -> operand.Range)
    |> LineBreakConvention.checkUniformPlacement src
    for operand in operands do checkConditionLayout src operand

/// The condition with the parentheses wrapped round the whole of it taken off.
///
/// Such a fence is not a group inside the condition. A group is asked for a
/// name so that the chain holding it goes back to reading in one line, and
/// there is no chain here beside it: what it holds is the condition entire.
/// Naming it would move the same operands under a `let` and leave the `if`
/// exactly as long as it was.
///
/// So the two spellings have to answer alike, as `elif` and `else if` do:
///
/// ```fsharp
/// if (a = 1 || b = 2 || c = 3) then  // parenthesised
/// if a = 1 || b = 2 || c = 3 then    // not
/// ```
let rec private unfence expr =
  match expr with
  | SynExpr.Paren(expr = inner) -> unfence inner
  | _ -> expr

/// Every operand of a `when` guard must either share one line or each sit on
/// one of its own, as the operands of an `if` condition do.
///
/// What is not asked of a guard is the name an `if` condition is asked for once
/// a parenthesised group runs past its line. There is nowhere in a match clause
/// to put that name: a `let` cannot stand ahead of the guard, and the group
/// commonly reads the very value the pattern bound, so lifting it out of the
/// match is closed off too. A guard answers for its gaps and for nothing else.
let rec checkGuardLayout src expr =
  match expr with
  | SynExpr.Paren(expr = inner) ->
    checkGuardLayout src inner
  | _ ->
    let operands = flattenInfixChain expr
    operands
    |> List.map (fun (operand: SynExpr) -> operand.Range)
    |> LineBreakConvention.checkUniformPlacement src
    for operand in operands do checkGuardLayout src operand

/// An `elif` chain hands its `else` to the nested link that ends it, so every
/// link above that one has an else expression without an `else` keyword of its
/// own. Only a link holding neither is the one truly missing its else, and the
/// absent expression says so on its own, whatever line the chain is laid on.
let check src ifExpr thenExpr (elseExpr: Option<SynExpr>) range trivia =
  if isStrict then
    checkBranchLayout src ifExpr thenExpr elseExpr range trivia
    checkConditionLayout src (unfence ifExpr)
    match elseExpr with
    | Some elseBody ->
      checkKeywordSpacing src ifExpr thenExpr elseBody trivia
    | None ->
      reportWarn src
        (trivia: SynExprIfThenElseTrivia).IfToThenRange
        "Add else expression"
  else
    ()