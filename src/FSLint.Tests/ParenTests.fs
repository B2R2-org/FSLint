namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type ParenTests() =

  let goodEmptyTest = """()"""

  let badEmptyTest = """( )"""

  let goodBracketSpacingTest = """(1, 2)"""

  let badBracketSpacingTest = """( 1, 2 )"""

  [<TestMethod>]
  member _.``[Paren] Paren Empty Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodEmptyTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badEmptyTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Paren] Paren Bracket Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBracketSpacingTest)
    ) |> ignore