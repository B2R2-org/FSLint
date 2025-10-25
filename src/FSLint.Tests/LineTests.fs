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

  let goodTrailingWhiteSpaceTest = """
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
    assertFSLintSuccess Constants.FakeFsPath goodLine80Test
    assertFSLintFailure Constants.FakeFsPath badLine80Test

  [<TestMethod>]
  member _.``Trailing Whitespace Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTrailingWhiteSpaceTest
    assertFSLintFailure Constants.FakeFsPath badTrailingWhiteSpaceTest
