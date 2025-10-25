namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ClassMemberTests() =

  let goodSpacingInfixParenTest = """
type TestClass() =
  static member inline (+) (param1, param2) = value
"""

  let badSpacingInfixParenTest = """
type TestClass() =
  static member inline (+)(param1, param2) = value
"""

  let goodSpacingFunctionParenTest = """
type TestClass() =
  member _.Create(param1, param2) = value
"""

  let badSpacingFunctionParenTest = """
type TestClass() =
  member _.Create (param1, param2) = value
"""

  let goodSelfIdentifierUnderscoreTest = """
type Class() =
  member _.A(p1, p2) = _.B()
"""

  let badSelfIdentifierUnderscoreTest = """
type Class() =
  member __.A(p1, p2) = __.B()
"""

  let goodSelfIdentifierUnusedTest = """
type Class() =
  member this.A(p1, p2) = this.B()
"""

  let badSelfIdentifierUnusedTest = """
type Class() =
  member this.A(p1, p2) = B()
"""

  [<TestMethod>]
  member _.``[ClassMember] Between Infix and Paren Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodSpacingInfixParenTest
    assertFSLintFailure Constants.FakeFsPath badSpacingInfixParenTest

  [<TestMethod>]
  member _.``[ClassMember] Between Function and Paren Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodSpacingFunctionParenTest
    assertFSLintFailure Constants.FakeFsPath badSpacingFunctionParenTest

  [<TestMethod>]
  member _.``[ClassMember] Self Identifier Double Underscore Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodSelfIdentifierUnderscoreTest
    assertFSLintFailure Constants.FakeFsPath badSelfIdentifierUnderscoreTest

  [<TestMethod>]
  member _.``[ClassMember] Self Identifier Unused Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodSelfIdentifierUnusedTest
    assertFSLintFailure Constants.FakeFsPath badSelfIdentifierUnusedTest