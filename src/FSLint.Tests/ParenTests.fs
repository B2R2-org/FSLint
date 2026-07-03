namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ParenTests() =

  let goodEmptyTest = """()"""

  let badEmptyTest = """( )"""

  let goodBracketSpacingTest = """(1, 2)"""

  let badBracketSpacingTest = """( 1, 2 )"""

  let goodTraitCallTest =
    """
let inline callM (x: ^T) = (^T: (member M: int) x)
"""

  [<TestMethod>]
  member _.``[Paren] Paren Empty Test``() =
    lint goodEmptyTest
    lintAssert badEmptyTest

  [<TestMethod>]
  member _.``[Paren] Paren Bracket Spacing Test``() =
    lint goodBracketSpacingTest
    lintAssert badBracketSpacingTest

  [<TestMethod>]
  member _.``[Paren] Trait Call No False Positive Test``() =
    lint goodTraitCallTest