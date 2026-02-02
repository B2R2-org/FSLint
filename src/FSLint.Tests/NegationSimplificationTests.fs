namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type NegationSimplificationTests() =

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a = b)``() =
    let code = "let f a b = not (a = b)\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a <> b)``() =
    let code = "let f a b = not (a <> b)\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a > b)``() =
    let code = "let f a b = not (a > b)\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a >= b)``() =
    let code = "let f a b = not (a >= b)\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a < b)``() =
    let code = "let f a b = not (a < b)\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a <= b)``() =
    let code = "let f a b = not (a <= b)\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - in if expression``() =
    let code =
      "let f x =\n" +
      "  if not (x > 0) then\n" +
      "    printfn \"negative\"\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - nested expression``() =
    let code = "let f a b c = a + not (b = c)\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Good - using opposite operator``() =
    let code = "let f a b = a <> b\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[NegationSimplification] Good - not with non-comparison``() =
    let code = "let f a = not (isValid a)\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[NegationSimplification] Good - not with boolean literal``() =
    let code = "let f = not true\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[NegationSimplification] Good - not with complex expression``() =
    let code = "let f a b = not (a > 0 && b < 0)\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[NegationSimplification] Error - pipeline not``() =
    let code = "let f a b = a = b |> not\n"
    Assert.ThrowsException<LintException>(fun () ->
    linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - pipeline with paren not``() =
    let code = "let f a b = (a = b) |> not\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Error - pipeline not ``() =
    let code = "let f a b = a > b |> not\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[NegationSimplification] Good - pipeline non-comparison``() =
    let code = "let f a = isValid a |> not\n"
    linterForFs.Lint(FakeFsPath, code)