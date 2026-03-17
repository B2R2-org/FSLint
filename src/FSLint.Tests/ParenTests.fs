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
    lint goodEmptyTest
    lintAssert badEmptyTest

  [<TestMethod>]
  member _.``[Paren] Paren Bracket Spacing Test``() =
    lint goodBracketSpacingTest
    lintAssert badBracketSpacingTest