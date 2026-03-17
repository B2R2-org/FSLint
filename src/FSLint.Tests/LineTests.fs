namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type LineTests() =

  let goodOnes = String.replicate 72 "1"

  let badOnes = String.replicate 73 "1"

  let goodLine80Test = $"""
let x = {goodOnes}
"""

  let badLine80Test = $"""
let x = {badOnes}
"""

  let goodTrailingWhiteSpaceTest = $"""
type A() =
  member _.X = 42
"""

  let space = " "

  let badTrailingWhiteSpaceTest = $"""
type A() ={space}
  member _.X = 42
"""

  [<TestMethod>]
  member _.``80 Line Test``() =
    lint goodLine80Test
    lintAssert badLine80Test

  [<TestMethod>]
  member _.``Trailing Whitespace Test``() =
    lint goodTrailingWhiteSpaceTest
    lintAssert badTrailingWhiteSpaceTest
