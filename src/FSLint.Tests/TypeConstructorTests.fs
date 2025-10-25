namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TypeConstructorTests() =

  let goodConstructorSpacingTest = """new TestClass(param1, param2)"""

  let badConstructorSpacingTest = """new TestClass (param1, param2)"""

  [<TestMethod>]
  member _.``[TypeConstructor] Between Infix and Paren Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodConstructorSpacingTest
    assertFSLintFailure Constants.FakeFsPath badConstructorSpacingTest
