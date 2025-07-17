namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type ClassMemberTests () =

  let goodSpacingInfixParenTest = """
type TestClass () =
  static member inline (+) (param1, param2) = value
"""

  let badSpacingInfixParenTest = """
type TestClass () =
  static member inline (+)(param1, param2) = value
"""

  let goodSpacingFunctionParenTest = """
type TestClass () =
  member _.Create(param1, param2) = value
"""

  let badSpacingFunctionParenTest = """
type TestClass () =
  member _.Create (param1, param2) = value
"""

  [<TestMethod>]
  member _.``[ClassMember] Between Infix and Paren Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodSpacingInfixParenTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badSpacingInfixParenTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ClassMember] Between Function and Paren Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodSpacingFunctionParenTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badSpacingFunctionParenTest
     ) |> ignore