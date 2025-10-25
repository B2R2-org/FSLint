namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TupleTests() =

  let goodCommaSpacingTest = """[ 1, 2, 3 ]"""

  let badSpacingAfterCommaTest = """[ 1,2, 3 ]"""

  let badSpacingBeforeCommaTest = """[ 1 , 2, 3 ]"""

  [<TestMethod>]
  member _.``[Tuple] Comma Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodCommaSpacingTest
    assertFSLintFailure Constants.FakeFsPath badSpacingBeforeCommaTest
    assertFSLintFailure Constants.FakeFsPath badSpacingAfterCommaTest