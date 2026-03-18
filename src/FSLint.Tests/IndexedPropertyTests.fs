namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type IndexedPropertyTests() =

  let goodIndexedPropertyTest = """src[1]"""

  let badIndexedPropertySpacingFromAppTest = """src [1]"""

  let badIndexedPropertyBracketSpacingTest = """src[ 1 ]"""

  let goodIndexedPropertyHasOpmTest = """src[expr1..opm..expr2]"""

  let badIndexedPropertyHasOpmTest = """src[expr1 .. opm .. expr2]"""

  [<TestMethod>]
  member _.``[IndexedProperty] Indexed Property Spacing From App Test``() =
    lint goodIndexedPropertyTest
    lintAssert badIndexedPropertySpacingFromAppTest

  [<TestMethod>]
  member _.``[IndexedProperty] Indexed Property Bracket Spacing Test``() =
    lintAssert badIndexedPropertyBracketSpacingTest

  [<TestMethod>]
  member _.``[IndexedProperty] Indexed Property Range Operator Test``() =
    lint goodIndexedPropertyHasOpmTest
    lintAssert badIndexedPropertyHasOpmTest