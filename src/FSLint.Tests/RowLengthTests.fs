namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type RowLengthTests() =

  /// The rule's budget. A body of this many rows keeps to it; one more does
  /// not.
  let budget = 42

  /// `n` rows of plain code, each row standing on its own.
  let codeRows n =
    [ for i in 1 .. n -> sprintf "  let v%02d = acc + %d\n" i i ]
    |> String.concat ""

  /// `n` rows of a table, each row an entry.
  let tableRows indent n =
    [ for i in 1 .. n -> sprintf "%s\"r%02d\"\n" indent i ]
    |> String.concat ""

  /// How many times the rule answered for the source.
  let reports source =
    lintErrors source
    |> List.filter (fun e -> e.Message = "Split into smaller functions")
    |> List.length

  /// A body of exactly `rows` rows: the last row carries the result.
  let plainBody rows =
    "let f acc =\n" + codeRows (rows - 1) + sprintf "  v%02d\n" (rows - 1)

  /// The budget is measured to the row, and both sides of it are checked.
  [<TestMethod>]
  member _.``[RowLength] Budget Boundary Test``() =
    Assert.AreEqual<int>(0, reports (plainBody budget))
    Assert.AreEqual<int>(1, reports (plainBody (budget + 1)))

  /// A table is no easier to read for being cut in three, and there is no
  /// smaller function to take out of one, so a body that is one literal
  /// written out is passed over however far down the page it runs.
  [<TestMethod>]
  member _.``[RowLength] Data Literal Test``() =
    let list = "let table =\n  [ \"r00\"\n" + tableRows "    " 60 + "  ]\n"
    let array = "let table =\n  [| \"r00\"\n" + tableRows "     " 60 + "  |]\n"
    let record =
      "type R = { A: int; B: int }\n\nlet value =\n  { A = 0\n"
      + ([ for i in 1 .. 60 -> sprintf "    B = %d\n" i ] |> String.concat "")
      + "    A = 1 }\n"
    Assert.AreEqual<int>(0, reports list)
    Assert.AreEqual<int>(0, reports array)
    Assert.AreEqual<int>(0, reports record)

  /// A literal handed straight to what shapes it is still the literal being
  /// read. That step is taken only where the call takes the one argument
  /// beside it: `f a b` nests to the left, so following the argument of a call
  /// whose head is itself a call walks to the last argument rather than into
  /// the data, and everything before it would go unread.
  [<TestMethod>]
  member _.``[RowLength] Shaped Literal Test``() =
    let shaped =
      "let shape v = v\n\nlet table =\n  shape [ \"r00\"\n"
      + tableRows "          " 60 + "        ]\n"
    let twoArgs =
      "let two a b = a + List.length b\n\nlet called =\n  two\n"
      + "    (let x = 1\n     let y = 2\n     x + y)\n    [ 0\n"
      + ([ for i in 1 .. 60 -> sprintf "      %d\n" i ] |> String.concat "")
      + "      61 ]\n"
    Assert.AreEqual<int>(0, reports shaped)
    Assert.AreEqual<int>(1, reports twoArgs)

  /// `seq { ... }` names a table as much as `[ ... ]` does; which brackets
  /// were typed says nothing about what stands in them. `async` and `task`
  /// sequence real work, which is what the rule is here to measure.
  [<TestMethod>]
  member _.``[RowLength] Computation Expression Test``() =
    let table =
      [ for i in 1 .. 60 -> sprintf "    yield \"s%02d\"\n" i ]
      |> String.concat ""
    let work =
      [ for i in 1 .. 60 -> sprintf "    let v%02d = url + %d\n" i i ]
      |> String.concat ""
    Assert.AreEqual<int>(0, reports ("let f () =\n  seq {\n" + table + "  }\n"))
    Assert.AreEqual<int>(1,
      reports ("let f url =\n  async {\n" + work + "    return v60\n  }\n"))
    Assert.AreEqual<int>(1,
      reports ("let f url =\n  task {\n" + work + "    return v60\n  }\n"))

  /// A `match` is an enumeration of patterns, read the way a table is. Its
  /// arms are never counted, however many of them there are.
  [<TestMethod>]
  member _.``[RowLength] Match Arm Count Test``() =
    let arms =
      [ for i in 1 .. 60 -> sprintf "  | %d -> \"m%02d\"\n" i i ]
      |> String.concat ""
    Assert.AreEqual<int>(0,
      reports ("let f op =\n  match op with\n" + arms + "  | _ -> \"?\"\n"))

  /// What can genuinely run long is the body standing under an arrow, and that
  /// one lifts out into a function of its own. The report names that body
  /// rather than the binding, since naming the binding says nothing about
  /// which arm to look at.
  [<TestMethod>]
  member _.``[RowLength] Match Clause Body Test``() =
    let source =
      "let f op acc =\n  match op with\n  | 0 ->\n"
      + ([ for i in 1 .. 60 -> sprintf "    let v%02d = acc + %d\n" i i ]
         |> String.concat "")
      + "    v60\n  | _ ->\n    acc\n"
    lintErrors source
    |> List.filter (fun e -> e.Message = "Split into smaller functions")
    |> fun errors ->
      Assert.AreEqual<int>(1, errors.Length)
      (* named at the clause body, not at `f` on the first row *)
      Assert.AreEqual<int>(4, errors.Head.Range.StartLine)

  /// The exemption for a literal reaches a binding body and no further. A
  /// table standing under an arrow can be lifted out to a binding of its own,
  /// where a table already standing as one cannot.
  [<TestMethod>]
  member _.``[RowLength] Clause Body Table Test``() =
    let source =
      "let f op =\n  match op with\n  | 0 ->\n    [| \"r00\"\n"
      + tableRows "       " 60 + "    |]\n  | _ ->\n    [||]\n"
    Assert.AreEqual<int>(1, reports source)

  /// A `try ... with` divides the same way a match does, and a
  /// `try ... finally` has two bodies that answer separately.
  [<TestMethod>]
  member _.``[RowLength] Try Body Test``() =
    let rows =
      [ for i in 1 .. 60 -> sprintf "    let v%02d = acc + %d\n" i i ]
      |> String.concat ""
    Assert.AreEqual<int>(1,
      reports ("let f acc =\n  try\n" + rows + "    v60\n  with _ ->\n    0\n"))
    Assert.AreEqual<int>(1,
      reports ("let f acc =\n  try\n" + rows + "    v60\n  finally\n    ()\n"))

  /// A binding written inside another is passed over: its rows are already
  /// counted in the body holding it, and the two are not two lengths but one.
  [<TestMethod>]
  member _.``[RowLength] Nested Binding Test``() =
    let source =
      "let outer acc =\n  let inner acc =\n"
      + ([ for i in 1 .. 60 -> sprintf "    let v%02d = acc + %d\n" i i ]
         |> String.concat "")
      + "    v60\n  inner acc\n"
    lintErrors source
    |> List.filter (fun e -> e.Message = "Split into smaller functions")
    |> fun errors ->
      Assert.AreEqual<int>(1, errors.Length)
      Assert.AreEqual<int>(1, errors.Head.Range.StartLine)

  /// What introduced the body does not matter; that it runs does. A binding
  /// taking no parameters is a function all the same once its body is code.
  [<TestMethod>]
  member _.``[RowLength] Parameterless Binding Test``() =
    let source =
      "let computed =\n  let acc = 7\n"
      + ([ for i in 1 .. 60 -> sprintf "  let v%02d = acc + %d\n" i i ]
         |> String.concat "")
      + "  v60\n"
    Assert.AreEqual<int>(1, reports source)
