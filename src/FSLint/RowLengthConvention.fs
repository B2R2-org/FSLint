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

/// What a pipe hands on, taken from the left. `a |> f` parses as
/// `App(App(|>, a), f)`, so what is being piped sits beside the operator rather
/// than at the top, where reading the shape of an expression would look for it.
let private (|PipedFrom|_|) expr =
  match expr with
  | SynExpr.App(funcExpr =
                  SynExpr.App(isInfix = true
                              funcExpr =
                                SynExpr.LongIdent(
                                  longDotId = SynLongIdent(id = [ op ]))
                              argExpr = piped)) when
      op.idText.StartsWith "op_PipeRight" ->
    Some piped
  | _ ->
    None

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
/// still the literal being read. A `let` standing in front of one only names
/// something the table uses, and an object expression is a type written where
/// an expression stands, whose members answer for themselves.
///
/// A table handed on by a pipe is the table still: `[| ... |] |> ofElements`
/// says what `ofElements [| ... |]` says, and which way round it was written
/// tells nothing about what is being read.
///
/// A conditional is a table too where both of its branches are: which of two
/// tables to hand back is not the sort of branching this rule is here to
/// measure. One branch of code makes it code again.
///
/// A string spelled out down the page is a table of its own: help text, a
/// banner, a sample of source. What is long about one is what it says, and
/// there is no smaller function inside a quotation mark.
///
/// What stands inside is not looked at. An entry running to several rows, a
/// callback, a `for` that fills in the tail -- none of it changes that the
/// whole is a table, and taking any of it out leaves the table no shorter.
let rec private isDataLiteral = function
  | SynExpr.Const(constant = SynConst.String _)
  | SynExpr.Record _
  | SynExpr.AnonRecd _
  | SynExpr.ArrayOrList _
  | SynExpr.ArrayOrListComputed _
  | SynExpr.ObjExpr _ ->
    true
  | SynExpr.App(funcExpr = SynExpr.Ident builder
                argExpr = SynExpr.ComputationExpr _) ->
    (* `seq { ... }` names a table as much as `[ ... ]` does; which brackets
       were typed says nothing about what stands in them. `async` and `task`
       sequence real work, which is what the rule is here to measure. *)
    builder.idText = "seq"
  | SynExpr.Paren(expr = inner)
  | SynExpr.Typed(expr = inner)
  | SynExpr.LetOrUse(body = inner) ->
    isDataLiteral inner
  | SynExpr.IfThenElse(thenExpr = thenBranch; elseExpr = Some elseBranch) ->
    isDataLiteral thenBranch && isDataLiteral elseBranch
  | PipedFrom piped ->
    isDataLiteral piped
  | SynExpr.App(funcExpr = head; argExpr = inner) ->
    isShaper head && isDataLiteral inner
  | _ ->
    false

/// True when the binding is one test. A test is a scenario: setting up,
/// running and checking, in that order, and cutting it in three gives three
/// names that mean nothing outside the test they came from.
let private isTest attrs =
  attrs
  |> List.exists (fun (lst: SynAttributeList) ->
    lst.Attributes
    |> List.exists (fun attr ->
      let SynLongIdent(id = lid) = attr.TypeName
      lid |> List.exists (fun id -> id.idText = "TestMethod")
    )
  )

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

/// What the body comes down to, once everything leading up to it is stepped
/// over: names bound, statements run, and what a pipe hands on. A `try` whose
/// answer is adjusted on the way out is still a `try`.
///
/// A `let` binding the answer and a body that only hands the name on come to
/// the same thing, so the value is read in the body's place. Where the body
/// does anything else, it is the body that answers.
///
/// Only what shape the body has is read this way. How long it is stays a
/// question about the whole body, everything stepped over included.
let rec private tailOf (expr: SynExpr) =
  match expr with
  | SynExpr.LetOrUse(bindings = [ SynBinding(expr = value) ]; body = body) ->
    (* `let ins = match ... with ... in ins |> f` names the answer and hands
       it straight on. Where the body comes down to nothing but a name, the
       substance is what the name was bound to. *)
    match tailOf body with
    | SynExpr.Ident _ -> tailOf value
    | tail -> tail
  | SynExpr.LetOrUse(body = body)
  | SynExpr.Sequential(expr2 = body) ->
    tailOf body
  | PipedFrom piped ->
    tailOf piped
  | _ ->
    expr

/// How many rows a `match` or a `while` may run to before the body holding it
/// stops answering for its own length.
let [<Literal>] private MaxEnumerationRows = 20

/// True when the body reaches a `match` or a `while` that runs past that.
///
/// A long `match` is an enumeration of patterns and a long `while` is one pass
/// over a stream written out. Both take their length from how many cases or
/// steps there are, and neither is any shorter for the body around it being cut
/// in two: splitting one renames the enumeration rather than shortening it.
/// Where one of them runs, it is what the body is about, however it was
/// threaded in -- standing as the answer, bound to a name and handed on, or run
/// as one statement between two others.
///
/// A short one is not. `match x with | 0 -> a | _ -> b` in front of forty rows
/// of code leaves those forty rows exactly as long as they were, and they are
/// what the budget is for.
///
/// What the body reaches is whatever its rows were counted from: what a name
/// is bound to as much as what is done with it, statements sequenced, pipes
/// threaded, the inside of parentheses and annotations, the bodies of loops and
/// of `try`, and either branch of a conditional. A helper written inside the
/// body is the body's own rows, so a long `match` inside one answers for it --
/// splitting the caller would only move the helper, which is already split.
///
/// What stands under an arrow, inside a lambda or beside a call as its
/// argument is not reached -- searching there would turn up a `match` in almost
/// every body and leave nothing measured at all.
let rec private holdsEnumeration (expr: SynExpr) =
  let rows (e: SynExpr) = e.Range.EndLine - e.Range.StartLine + 1
  match expr with
  | SynExpr.Match _
  | SynExpr.MatchLambda _
  | SynExpr.While _ when rows expr > MaxEnumerationRows ->
    true
  | SynExpr.LetOrUse(bindings = bindings; body = body) ->
    holdsEnumeration body
    || bindings
       |> List.exists (fun (SynBinding(expr = value)) -> holdsEnumeration value)
  | SynExpr.Paren(expr = inner)
  | SynExpr.Typed(expr = inner)
  | SynExpr.Do(expr = inner)
  | SynExpr.While(doExpr = inner)
  | SynExpr.For(doBody = inner)
  | SynExpr.ForEach(bodyExpr = inner)
  | SynExpr.TryWith(tryExpr = inner) ->
    holdsEnumeration inner
  | SynExpr.Sequential(expr1 = first; expr2 = second)
  | SynExpr.TryFinally(tryExpr = first; finallyExpr = second)
  | SynExpr.IfThenElse(thenExpr = first; elseExpr = Some second) ->
    holdsEnumeration first || holdsEnumeration second
  | PipedFrom piped ->
    holdsEnumeration piped
  | _ ->
    false

/// Measures one stretch, or the stretches standing inside it.
///
/// A body reaching a long `match` or `while` answers for nothing: neither the
/// arms nor what stands under them is measured. `try ... with`
/// divides the same way; `try ... finally` has two bodies that are not
/// patterns and answer as they stand.
let rec private checkRows src (expr: SynExpr) (target: range) =
  if holdsEnumeration expr then
    ()
  else
    match tailOf expr with
    | SynExpr.TryWith(tryExpr = tryBody) ->
      checkRows src tryBody target
    | SynExpr.TryFinally(tryExpr = tryBody; finallyExpr = finallyBody) ->
      checkRows src tryBody target
      checkRows src finallyBody target
    | _ ->
      let rows = expr.Range.EndLine - expr.Range.StartLine + 1
      if rows <= MaxBodyRows then () else reportLongFunction src target

/// A binding whose body runs past the budget is asked to be broken up.
///
/// A body that is one literal written out is passed over, and is not noted as
/// covered either: a binding written inside a table still answers for its own
/// length, since that one can be lifted out where the table cannot. A test is
/// passed over the same way.
///
/// A binding written inside another is passed over. Its rows are already
/// counted in the body holding it, and the two are not two lengths but one;
/// the outermost is where the splitting has to start.
let check src (binding: SynBinding) =
  let SynBinding(attributes = attrs; headPat = pat; expr = body) = binding
  if not isStrict then
    ()
  elif isTest attrs then
    ()
  elif isDataLiteral body then
    ()
  elif isInsideCoveredFunction body.Range then
    ()
  else
    noteCoveredFunction body.Range
    checkRows src body (reportTarget pat)
