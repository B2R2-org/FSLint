module B2R2.FSLint.RowLengthConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

/// How many rows a binding body may run to. Fixed here for now; it moves to
/// the settings once they carry it.
let [<Literal>] private MaxBodyRows = 42

/// True when the head of a call is a plain name, so that what stands beside it
/// is the call's only argument.
///
/// `f a b` nests to the left, as `App(App(f, a), b)`. Following the argument of
/// a call whose head is itself a call therefore walks to the last argument
/// rather than into the data, and everything before it goes unread.
let private isShaper = function
  | SynExpr.Ident _ | SynExpr.LongIdent _ -> true
  | _ -> false

/// True when the body is one piece of data written out rather than code.
///
/// A record or a literal spread down the page is a table, and a table is no
/// easier to read for being cut in three. There is no smaller function to take
/// out of one: an alphabetical table halved gives two halves and no name worth
/// having, and a table read by its index cannot be halved at all. What is long
/// about one is its width on the page, and the line budget answers that.
///
/// Which side of an `=` it was written on makes no difference: `let regs () =
/// [| ... |]` names the same table `let regs = [| ... |]` does.
///
/// An annotation and a pair of parentheses fence nothing off, and a literal
/// handed straight to what shapes it -- `set [ ... ]`, `dict [ ... ]` -- is
/// still the literal being read.
///
/// What stands inside is not looked at. An entry running to several rows, a
/// callback, a `for` that fills in the tail -- none of it changes that the
/// whole is a table, and taking any of it out leaves the table no shorter.
let rec private isDataLiteral = function
  | SynExpr.Record _
  | SynExpr.AnonRecd _
  | SynExpr.ArrayOrList _
  | SynExpr.ArrayOrListComputed _ ->
    true
  | SynExpr.App(funcExpr = SynExpr.Ident builder
                argExpr = SynExpr.ComputationExpr _) ->
    (* `seq { ... }` names a table as much as `[ ... ]` does; which brackets
       were typed says nothing about what stands in them. `async` and `task`
       sequence real work, which is what the rule is here to measure. *)
    builder.idText = "seq"
  | SynExpr.Paren(expr = inner)
  | SynExpr.Typed(expr = inner) ->
    isDataLiteral inner
  | SynExpr.App(funcExpr = head; argExpr = inner) ->
    isShaper head && isDataLiteral inner
  | _ ->
    false

/// Where the report belongs: the identifier the binding is known by. The whole
/// body is what is wrong, and underlining forty rows of it says nothing the
/// count has not already said. A binding named by a pattern rather than an
/// identifier is named at the pattern.
let private reportTarget (pat: SynPat) =
  match pat with
  | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
    (List.last ids).idRange
  | _ ->
    pat.Range

/// A binding whose body runs past the budget is asked to be broken up.
///
/// A body that is one literal written out is passed over, and is not noted as
/// covered either: a binding written inside a table still answers for its own
/// length, since that one can be lifted out where the table cannot.
///
/// A binding written inside another is passed over. Its rows are already
/// counted in the body holding it, and the two are not two lengths but one;
/// the outermost is where the splitting has to start.
/// Measures one stretch, or the stretches standing inside it.
///
/// A `match` is an enumeration of patterns, and an enumeration is read the way
/// a table is: looked up, not followed. Splitting one by its arms would have
/// the reader go through it twice, so the arms are never counted. What can
/// genuinely run long is the body standing under an arrow, and that one lifts
/// out into a function of its own -- so each clause body answers on its own,
/// and the report names that body rather than the binding, since naming the
/// binding says nothing about which arm to look at.
///
/// `try ... with` divides the same way, and `try ... finally` has two bodies
/// that answer separately.
///
/// The exemption for a literal is not carried in here. It reaches a binding
/// body and no further: a table standing under an arrow can be lifted out to a
/// binding of its own, where a table already standing as one cannot.
let rec private checkRows src (expr: SynExpr) (target: range) =
  match expr with
  | SynExpr.Match(clauses = clauses)
  | SynExpr.MatchLambda(matchClauses = clauses) ->
    for SynMatchClause(resultExpr = body) in clauses do
      checkRows src body body.Range
  | SynExpr.TryWith(tryExpr = tryBody; withCases = clauses) ->
    checkRows src tryBody tryBody.Range
    for SynMatchClause(resultExpr = body) in clauses do
      checkRows src body body.Range
  | SynExpr.TryFinally(tryExpr = tryBody; finallyExpr = finallyBody) ->
    checkRows src tryBody tryBody.Range
    checkRows src finallyBody finallyBody.Range
  | _ ->
    let rows = expr.Range.EndLine - expr.Range.StartLine + 1
    if rows <= MaxBodyRows then () else reportLongFunction src target

let check src (binding: SynBinding) =
  let SynBinding(headPat = pat; expr = body) = binding
  if not isStrict then
    ()
  elif isDataLiteral body then
    ()
  elif isInsideCoveredFunction body.Range then
    ()
  else
    noteCoveredFunction body.Range
    checkRows src body (reportTarget pat)
