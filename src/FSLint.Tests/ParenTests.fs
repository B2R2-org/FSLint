namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ParenTests() =

  let goodEmptyTest = """()"""

  let badEmptyTest = """( )"""

  let goodBracketSpacingTest = """(1, 2)"""

  let badBracketSpacingTest = """( 1, 2 )"""

  [<TestMethod>]
  member _.``[Paren] Paren Empty Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodEmptyTest
    assertFSLintFailure Constants.FakeFsPath badEmptyTest

  [<TestMethod>]
  member _.``[Paren] Paren Bracket Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badBracketSpacingTest