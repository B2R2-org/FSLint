namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

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

  [<TestMethod>]
  member _.``[ClassMember] Between Infix and Paren Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodSpacingInfixParenTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSpacingInfixParenTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ClassMember] Between Function and Paren Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodSpacingFunctionParenTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSpacingFunctionParenTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ClassMember] Self Identifier Double Underscore Test``() =
    linterForFs.Lint(FakeFsPath, goodSelfIdentifierUnderscoreTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSelfIdentifierUnderscoreTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ClassMember] Self Identifier Unused Test``() =
    linterForFs.Lint(FakeFsPath, goodSelfIdentifierUnusedTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSelfIdentifierUnusedTest)
     ) |> ignore