namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ClassMemberTests() =

  let goodSpacingInfixParenTest =
    """
type TestClass() =
  static member inline (+) (param1, param2) = value
"""

  let badSpacingInfixParenTest =
    """
type TestClass() =
  static member inline (+)(param1, param2) = value
"""

  let goodSpacingFunctionParenTest =
    """
type TestClass() =
  member _.Create(param1, param2) = value
"""

  let badSpacingFunctionParenTest =
    """
type TestClass() =
  member _.Create (param1, param2) = value
"""

  let goodSelfIdentifierUnderscoreTest =
    """
type Class() =
  member _.A(p1, p2) = _.B()
"""

  let badSelfIdentifierUnderscoreTest =
    """
type Class() =
  member __.A(p1, p2) = __.B()
"""

  let goodSelfIdentifierUnusedTest =
    """
type Class() =
  member this.A(p1, p2) = this.B()
"""

  let badSelfIdentifierUnusedTest =
    """
type Class() =
  member this.A(p1, p2) = B()
"""

  let goodMultiLineSpacingTest =
    """
type Class() =
  member _.A() =
    42

  member _.B() =
    42
"""

  let badMultiLineSpacingTest =
    """
type Class() =
  member _.A() =
    42


  member _.B() =
    42
"""

  let goodMultiLineSpacingInMemberTest =
    """
type Class() =
  member _.A() =
    let x = 42
    let y = 42
    x + y
"""

  let badMultiLineSpacingInMemberTest =
    """
type Class() =
  member _.A() =
    let x = 42


    let y = 42
    x + y
"""

  [<TestMethod>]
  member _.``[ClassMember] Between Infix and Paren Spacing Test``() =
    lint goodSpacingInfixParenTest
    lintAssert badSpacingInfixParenTest

  [<TestMethod>]
  member _.``[ClassMember] Between Function and Paren Spacing Test``() =
    lint goodSpacingFunctionParenTest
    lintAssert badSpacingFunctionParenTest

  [<TestMethod>]
  member _.``[ClassMember] Self Identifier Double Underscore Test``() =
    lint goodSelfIdentifierUnderscoreTest
    lintAssert badSelfIdentifierUnderscoreTest

  [<TestMethod>]
  member _.``[ClassMember] Self Identifier Unused Test``() =
    lint goodSelfIdentifierUnusedTest
    lintAssert badSelfIdentifierUnusedTest

  [<TestMethod>]
  member _.``[ClassMember] MultiLine Spacing In Type Test``() =
    lint goodMultiLineSpacingTest
    lintAssert badMultiLineSpacingTest

  [<TestMethod>]
  member _.``[ClassMember] MultiLine Spacing In Member Test``() =
    lint goodMultiLineSpacingInMemberTest
    lintAssert badMultiLineSpacingInMemberTest