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