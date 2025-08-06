namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type TypeCastTests() =

  let goodUpcastSpacingTest = """source :> target"""

  let badUpcastSpacingTest = """source:>target"""

  let goodDowncastSpacingTest = """source :?> target"""

  let badDowncastSpacingTest = """source:?>target"""

  [<TestMethod>]
  member _.``[TypeCast] Upcast Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodUpcastSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badUpcastSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[TypeCast] Downcast Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodDowncastSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badDowncastSpacingTest)
     ) |> ignore