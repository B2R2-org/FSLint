namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type AssignmentTests() =

  let goodNamedArgumentSpacingTest = """func (param = value)"""

  let badNamedArgumentSpacingTest = """func (param=value)"""


  [<TestMethod>]
  member _.``[Assignment] Named Argument Assignment Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodNamedArgumentSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badNamedArgumentSpacingTest)
     ) |> ignore