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
    assertFSLintSuccess Constants.FakeFsPath goodGenericArguSpacingTest
    assertFSLintFailure Constants.FakeFsPath badGenericArguSpacingTest

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Comma Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodGenericArguCommaTest
    assertFSLintFailure Constants.FakeFsPath badGenericArguCommaTest

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Star Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodGenericArguStarTest
    assertFSLintFailure Constants.FakeFsPath badGenericArguStarTest