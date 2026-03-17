namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type TypeConstructorTests() =

  let goodConstructorSpacingTest = """new TestClass(param1, param2)"""

  let badConstructorSpacingTest = """new TestClass (param1, param2)"""

  [<TestMethod>]
  member _.``[TypeConstructor] Between Infix and Paren Spacing Test``() =
    lint goodConstructorSpacingTest
    lintAssert badConstructorSpacingTest
