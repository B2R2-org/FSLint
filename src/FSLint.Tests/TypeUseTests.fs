namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type TypeUseTests() =

  let goodGenericArguSpacingTest = """func<genericArgu>"""

  let badGenericArguSpacingTest = """func <genericArgu>"""

  let goodGenericArguCommaTest = """func<type1, type2>"""

  let badGenericArguCommaTest = """func<type1,type2>"""

  let goodGenericArguStarTest = """func<type1 * type2>"""

  let badGenericArguStarTest = """func<type1*type2>"""

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodGenericArguSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badGenericArguSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Comma Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodGenericArguCommaTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badGenericArguCommaTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Star Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodGenericArguStarTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badGenericArguStarTest)
     ) |> ignore