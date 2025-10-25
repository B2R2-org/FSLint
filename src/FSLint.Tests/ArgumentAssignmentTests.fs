namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type AssignmentTests() =

  let goodNamedArgumentSpacingTest = """func (param = value)"""

  let badNamedArgumentSpacingTest = """func (param=value)"""

  [<TestMethod>]
  member _.``[Assignment] Named Argument Assignment Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodNamedArgumentSpacingTest
    assertFSLintFailure Constants.FakeFsPath badNamedArgumentSpacingTest
