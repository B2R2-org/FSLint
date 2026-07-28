namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TypeUseTests() =

  let goodGenericArguCommaTest = """func<type1, type2>"""

  let badGenericArguCommaTest = """func<type1,type2>"""

  let goodGenericArguStarTest = """func<type1 * type2>"""

  let badGenericArguStarTest = """func<type1*type2>"""

  let goodWrappedGenericArguTest =
    """
let t =
  Dictionary<int,
             string>()
"""

  let badWrappedGenericArguTest =
    """
let t =
  Dictionary<int ,
             string>()
"""

  let badWrappedNestedGenericArguTest =
    """
let t =
  [ typeof<Dictionary<int ,
                      string>> ]
"""

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Comma Spacing Test``() =
    lint goodGenericArguCommaTest
    lintAssert badGenericArguCommaTest

  [<TestMethod>]
  member _.``[TypeUse] Generic Argument Star Spacing Test``() =
    lint goodGenericArguStarTest
    lintAssert badGenericArguStarTest

  [<TestMethod>]
  member _.``[TypeUse] Wrapped Generic Argument Comma Spacing Test``() =
    lint goodWrappedGenericArguTest
    lintAssert badWrappedGenericArguTest
    lintAssert badWrappedNestedGenericArguTest