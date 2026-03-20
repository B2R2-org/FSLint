namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TypeUseTests() =

  let goodGenericArguCommaTest = """func<type1, type2>"""

  let badGenericArguCommaTest = """func<type1,type2>"""

  let goodGenericArguStarTest = """func<type1 * type2>"""

  let badGenericArguStarTest = """func<type1*type2>"""

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Comma Spacing Test``() =
    lint goodGenericArguCommaTest
    lintAssert badGenericArguCommaTest

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Star Spacing Test``() =
    lint goodGenericArguStarTest
    lintAssert badGenericArguStarTest