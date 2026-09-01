namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type DeclarationTests() =

  let goodBindingSingleSpacingTest =
    """
let foo = 1

let bar = 2
"""

  let goodBindingNoSpacingTest =
    """
let foo = 1
let bar = 2
"""

  let badTopBindingTooMuchSpacingTest =
    """
let foo = 1


let bar = 2
"""

  [<TestMethod>]
  member _.``[ModuleDeclaration] Top Binding Spacing Test``() =
    lint goodBindingSingleSpacingTest
    lint goodBindingNoSpacingTest

  [<TestMethod>]
  member _.``[ModuleDeclaration] Top Binding Too Much Spacing Test``() =
    lintAssert badTopBindingTooMuchSpacingTest

  [<TestMethod>]
  member _.``[Declaration] Single line fits in 80 columns``() =
    let code =
      """
let func = printfn "hello"
"""
    lint code

  [<TestMethod>]
  member _.``[Declaration] Short function on one line``() =
    let code =
      """
let add x y = x + y
"""
    lint code

  [<TestMethod>]
  member _.``[Declaration] Multi-line with long body - allowed``() =
    let code =
      """
let processData input =
  let step1 = transform input
  let step2 = validate step1
  compute step2
"""
    lint code

  [<TestMethod>]
  member _.``[Declaration] Computation expression on next line - good``() =
    let code =
      "let loop () =\n" +
      "  async {\n" +
      "    return 42\n" +
      "  }\n"
    lint code

  [<TestMethod>]
  member _.``[Declaration] Task expression on next line - good``() =
    let code =
      "let processData () =\n" +
      "  task {\n" +
      "    return! getData ()\n" +
      "  }\n"
    lint code

  [<TestMethod>]
  member _.``[Declaration] Seq expression on next line - good``() =
    let code =
      "let numbers =\n" +
      "  seq {\n" +
      "    yield 1\n" +
      "    yield 2\n" +
      "  }\n"
    lint code

  [<TestMethod>]
  member _.``[Declaration] Error - async on same line as equals``() =
    "let loop () = async {\n" +
    "  return 42\n" +
    "}\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[Declaration] Error - task on same line as equals``() =
    "let process () = task {\n" +
    "  return 1\n" +
    "}\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[Declaration] Error - seq on same line as equals``() =
    "let nums = seq {\n" +
    "  yield 1\n" +
    "}\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[Declaration] Rec function with async on next line - good``() =
    "let rec loop () =\n" +
    "  async {\n" +
    "    do! Async.Sleep 100\n" +
    "    return! loop ()\n" +
    "  }\n"
    |> lint

  /// A body already beside its '=' has nothing to answer for.
  [<TestMethod>]
  member _.``[Declaration] Body beside equals is left alone``() =
    "let cond = true\n" +
    "let a = if cond then 1 else 2\n"
    |> lint

  /// A body spread over lines of its own can never be joined, whatever its
  /// widest line reads.
  [<TestMethod>]
  member _.``[Declaration] Body needing its own lines is left alone``() =
    "let cond = true\n" +
    "let a =\n" +
    "  if cond then\n" +
    "    printfn \"one\"\n" +
    "    printfn \"two\"\n" +
    "  else\n" +
    "    ()\n"
    |> lint

  [<TestMethod>]
  member _.``[Declaration] Match body is left alone``() =
    "let cond = true\n" +
    "let a =\n" +
    "  match cond with\n" +
    "  | true -> 1\n" +
    "  | false -> 2\n"
    |> lint

  [<TestMethod>]
  member _.``[Declaration] Body one past the budget``() =
    let body = "someCall " + String.replicate 56 "x"
    "let someCall v = v\n" + "let exactly81 =\n  " + body + "\n"
    |> lint

  /// A comment between the '=' and the body would be swallowed by the join,
  /// and a comment trailing the body counts toward the width.
  [<TestMethod>]
  member _.``[Declaration] Comment holds the body down``() =
    "let cond = true\n" +
    "let a =\n" +
    "  (* keep me *)\n" +
    "  if cond then 1 else 2\n"
    |> lint

  [<TestMethod>]
  member _.``[Declaration] Trailing comment counts toward the budget``() =
    "let cond = true\n" +
    "let b =\n" +
    "  if cond then 1 else 2" +
    " (* a trailing note that pushes past the budget line *)\n"
    |> lint

  /// A directive inside cannot be closed over.
  [<TestMethod>]
  member _.``[Declaration] Directive holds the body down``() =
    "let d =\n" +
    "#if DEBUG\n" +
    "  1\n" +
    "#else\n" +
    "  2\n" +
    "#endif\n"
    |> lint

  /// A computation expression is held below the '=' by a rule of its own.
  [<TestMethod>]
  member _.``[Declaration] Computation expression is left alone``() =
    "let c =\n" +
    "  async { return 1 }\n"
    |> lint

  /// A group reaching across a directive holds different members in each
  /// build, so whether its bodies could all come up beside their keywords has
  /// a different answer per build. Only the demand that they agree, which
  /// every build can meet at once, is put to such a group; the demand that
  /// they all come up would contradict the build whose body cannot.
  [<TestMethod>]
  member _.``[Declaration] Conditional group is asked one thing``() =
    let clauses body =
      "let unop op e =\n" +
      "  match e with\n" +
      body +
      "#if ! HASHCONS\n" +
      "  | _ ->\n" +
      "    UnOp(op, e, null)\n" +
      "#else\n" +
      "  | _ ->\n" +
      "    let hc = HashConsingInfo()\n" +
      "    internExpr e hc (Expr.HashUnOp(op, e))\n" +
      "#endif\n"
    (* The clause beside its arrow disagrees with the one below, in the build
       that spreads its body over lines of its own. *)
    clauses "  | Num(n, _) -> ValueOptimizer.unop n op |> num\n"
    |> lintAssertMsg "Use consistent line breaks"
    (* Breaking it settles the matter, and no build asks for it back. *)
    clauses "  | Num(n, _) ->\n    ValueOptimizer.unop n op |> num\n"
    |> lint

  /// Each build is read on its own, so a group can agree in one of them and
  /// be mixed in the other. Only the build that mixes them is reported.
  [<TestMethod>]
  member _.``[Declaration] Conditional group agrees per build``() =
    let wide =
      "aRatherLongCallThatRunsPastTheBudgetOnceItSitsBesideItsKeyword"
      + "HereXX op e"
    let chain first thumb =
      "let f op e =\n  match e with\n" + first + "#if ! HASHCONS\n" + thumb
      + "#else\n  | _ ->\n    shortThree op e\n#endif\n"
    (* Every body below its keyword: both builds agree. *)
    chain "  | Num(n, _) ->\n    shortOne n op\n"
      ("  | _ ->\n    " + wide + "\n")
    |> lint
    (* The '#else' build reads one body beside its arrow and one below it, so
       that build alone is mixed and says so. *)
    chain "  | Num(n, _) -> shortOne n op\n" ("  | _ -> " + wide + "\n")
    |> lintErrors
    |> List.filter (fun e -> e.Message = "Use consistent line breaks")
    |> fun errors -> Assert.AreEqual<int>(1, errors.Length)

  /// What broke is the header rather than the body, and the parameter list
  /// answers for that elsewhere.
  [<TestMethod>]
  member _.``[Declaration] Broken header keeps the body check out``() =
    lintErrors ("let brokenHeader (a: int)\n" +
                "                 (b: int) =\n" +
                "  a + b\n")
    |> List.filter (fun e -> e.Range.StartLine = 3)
    |> fun errors -> Assert.AreEqual<int>(0, errors.Length)

  /// A body sent below its `=` opens two columns in from the keyword above it.
  [<TestMethod>]
  member _.``[Declaration] Body Indent Test``() =
    lint "let ok =\n  1\n"
    lint "let beside = 1\n"
    lint "let nested =\n  let inner =\n    2\n  inner\n"
    lint "let rec f x =\n  g x\n\nand g x =\n  f x\n"
    lintAssertMsg "Indent the body by two columns" "let far =\n    1\n"
    lintAssertMsg "Indent the body by two columns" "let odd =\n   1\n"
    lintAssertMsg "Indent the body by two columns" "let near =\n 1\n"

  /// The two columns are counted from the keyword and not from the margin, so
  /// an indented binding is read against where its own `let` stands. A body
  /// four columns in is right under a `let` two columns in and wrong under one
  /// standing at the margin.
  [<TestMethod>]
  member _.``[Declaration] Body Indent Is Read From The Keyword Test``() =
    lint ("module Inner =\n" +
          "  let ok entry =\n" +
          "    entry = 1\n")
    lintAssertMsg "Indent the body by two columns"
      ("module Inner =\n" +
       "  let private isUndefinedEntry entry =\n" +
       "     entry = 1 || entry = 2\n")
    lintAssertMsg "Indent the body by two columns"
      ("type Holder() =\n" +
       "  let bad =\n" +
       "      2\n" +
       "\n" +
       "  member _.X = bad\n")

  /// Once the header wraps, the lines under it continue the header and line up
  /// with something inside it. Where they line up is not read here.
  [<TestMethod>]
  member _.``[Declaration] Body Indent Skips A Wrapped Header Test``() =
    lint ("let wrapped (a: int)\n" +
          "            (b: int) =\n" +
          "      a + b\n")
    lint ("let alsoWrapped a\n" +
          "                b =\n" +
          "   a + b\n")

  /// What the parser reads as the body of a binding reached across a directive
  /// is whichever branch this build kept, and that branch opens where the
  /// shape around it put it rather than where the binding did.
  [<TestMethod>]
  member _.``[Declaration] Body Indent Skips A Directive Test``() =
    lint ("let load x =\n" +
          "#if DEBUG\n" +
          "  match x with\n" +
          "  | _ ->\n" +
          "#endif\n" +
          "    x\n")

  /// A type writes its shape below its `=` at the two columns a binding body
  /// takes, whatever that shape is.
  [<TestMethod>]
  member _.``[Declaration] Type Body Indent Test``() =
    lint "type Ok =\n  { A: int }\n"
    lint "type OkUni =\n  | A\n  | B\n"
    lint "type OkAbbrev =\n  int\n"
    lint "type OkClass() =\n  member _.X = 1\n"
    lint "type OkPriv =\n  private\n    { A: int }\n"
    lintAssertMsg "Indent the body by two columns"
      "type Far =\n    { A: int }\n"
    lintAssertMsg "Indent the body by two columns"
      "type Near =\n { A: int }\n"
    lintAssertMsg "Indent the body by two columns"
      "type Uni =\n    | A\n    | B\n"
    lintAssertMsg "Indent the body by two columns"
      "type Cls() =\n    member _.X = 1\n"

  /// An `and` link opens its own keyword, and is read against that one.
  [<TestMethod>]
  member _.``[Declaration] Type Body Indent Of An And Link Test``() =
    lint "type Left =\n  | X\n\nand Right =\n  | Y\n"
    lintAssertMsg "Indent the body by two columns"
      "type Left =\n  | X\n\nand Right =\n   | Y\n"

  /// A bracket left up beside the `=` fences the rows under it, and the first
  /// of them is where the body opens. Only that first row is read.
  [<TestMethod>]
  member _.``[Declaration] Fenced Body Indent Test``() =
    lint "type Ok = {\n  A: int\n  B: int\n}\n"
    lintAssertMsg "Indent the body by two columns"
      "type Far = {\n    A: int\n    B: int\n}\n"
    lintAssertMsg "Indent the body by two columns"
      "type Near = {\n A: int\n}\n"

  /// The two columns are counted from the keyword here too, so a fenced body
  /// under an indented `type` opens where that `type` stands and not at the
  /// margin. Four columns in is right under a `type` two columns in and wrong
  /// under one at the margin; two columns in is the other way round.
  [<TestMethod>]
  member _.``[Declaration] Fenced Body Indent Is Read From The Keyword``() =
    lint ("module Outer =\n" +
          "  type Ok = {\n" +
          "    A: int\n" +
          "  }\n")
    lintAssertMsg "Indent the body by two columns"
      ("module Outer =\n" +
       "  type Far = {\n" +
       "      A: int\n" +
       "  }\n")
    lintAssertMsg "Indent the body by two columns"
      ("module Outer =\n" +
       "  type Near = {\n" +
       "  A: int\n" +
       "  }\n")

  /// A body closing on anything but a bracket is not a fenced layout: what
  /// stands under it is the tail of something that ran past its line, and
  /// where that lines up is a question for whatever broke it.
  [<TestMethod>]
  member _.``[Declaration] Fenced Body Indent Reads Brackets Only Test``() =
    lint "let s = \"\"\"\ntext at column zero\n\"\"\"\n"
    lint "let f = List.map (fun y ->\n  y + 1\n)\n"
    lint "let g = max 1\n            2\n"

  /// A member opens a body with an `=` as a `let` does, and it is read the same
  /// way. Nothing about the type holding it changes the two columns.
  [<TestMethod>]
  member _.``[Declaration] Member Body Indent Test``() =
    lint ("type A() =\n" +
          "  member _.Ok =\n" +
          "    1\n")
    lint ("type A() =\n" +
          "  static member Ok =\n" +
          "    1\n")
    lint ("type A() =\n" +
          "  override _.ToString() =\n" +
          "    \"x\"\n")
    lintAssertMsg "Indent the body by two columns"
      ("type A() =\n" +
       "  member _.Far =\n" +
       "      1\n")
    lintAssertMsg "Indent the body by two columns"
      ("type A() =\n" +
       "  member _.Near =\n" +
       "   1\n")
    lintAssertMsg "Indent the body by two columns"
      ("type A() =\n" +
       "  static member Far =\n" +
       "      1\n")
    lintAssertMsg "Indent the body by two columns"
      ("type A() =\n" +
       "  override _.GetHashCode() =\n" +
       "       0\n")

  /// The two columns are counted from where the declaration's row begins, not
  /// from the keyword. An access modifier can stand in front of the keyword,
  /// and a member nested in an interface implementation begins further in.
  [<TestMethod>]
  member _.``[Declaration] Member Body Indent Is Read From The Row``() =
    lint ("type B(x: int) =\n" +
          "  private new() =\n" +
          "    B(0)\n" +
          "\n" +
          "  member _.X = x\n")
    lint ("open System\n" +
          "\n" +
          "type C() =\n" +
          "  interface IDisposable with\n" +
          "    member _.Dispose() =\n" +
          "      ()\n")
    lintAssertMsg "Indent the body by two columns"
      ("open System\n" +
       "\n" +
       "type C() =\n" +
       "  interface IDisposable with\n" +
       "    member _.Dispose() =\n" +
       "        ()\n")

  /// A comment is prose, and where it sits is the author's business. The row
  /// read is the first one holding code, so the same code passes or fails on
  /// its own column and not on where somebody put the comment above it.
  [<TestMethod>]
  member _.``[Declaration] Body Indent Passes Over Comments Test``() =
    lint ("type OkA =\n" +
          "   /// note\n" +
          "  | Activated\n" +
          "  | Deactivated\n")
    lint ("type OkC =\n" +
          "    (* note *)\n" +
          "  { A: int }\n")
    lint ("type OkD =\n" +
          "      (* a\n" +
          "         b *)\n" +
          "  { A: int }\n")
    lint ("let okLet =\n" +
          "    // note\n" +
          "  1\n")
    lint ("type OkFenced = {\n" +
          "    (* note *)\n" +
          "  A: int\n" +
          "}\n")
    lintAssertMsg "Indent the body by two columns"
      ("type BadA =\n" +
       "   /// note\n" +
       "   | Activated\n")
    lintAssertMsg "Indent the body by two columns"
      ("let badLet =\n" +
       "  // note\n" +
       "    1\n")

  /// The report points at the row holding the code, not at the comment above
  /// it: that row is what has to move.
  [<TestMethod>]
  member _.``[Declaration] Body Indent Reports The Code Row Test``() =
    lintErrors ("type BadA =\n" +
                "   /// note\n" +
                "   | Activated\n")
    |> List.filter (fun e -> e.Message = "Indent the body by two columns")
    |> fun errors ->
      Assert.AreEqual<int>(1, errors.Length)
      Assert.AreEqual<int>(3, errors.Head.Range.StartLine)
