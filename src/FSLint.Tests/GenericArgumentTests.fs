namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type GenericArgumentTests() =

  let goodGenericArguSpacingTest = """func<genericArgu>"""

  let badGenericArguSpacingTest = """func <genericArgu>"""

  let goodGenericArguCommaTest = """func<type1, type2>"""

  let badGenericArguCommaTest = """func<type1,type2>"""

  let goodGenericArguStarTest = """func<type1 * type2>"""

  let badGenericArguStarTest = """func<type1*type2>"""

  [<TestMethod>]
  member _.``[GenericArgument] Generic Argument Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodGenericArguSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badGenericArguSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[GenericArgument] Generic Argument Comma Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodGenericArguCommaTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badGenericArguCommaTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[GenericArgument] Generic Argument Star Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodGenericArguStarTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badGenericArguStarTest)
     ) |> ignore