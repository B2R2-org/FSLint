namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type TupleTests() =

  let goodCommaSpacingTest = """[ 1, 2, 3 ]"""

  let badSpacingAfterCommaTest = """[ 1,2, 3 ]"""

  let badSpacingBeforeCommaTest = """[ 1 , 2, 3 ]"""

  [<TestMethod>]
  member _.``[Tuple] Comma Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodCommaSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badSpacingBeforeCommaTest)
     ) |> ignore
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badSpacingAfterCommaTest)
     ) |> ignore