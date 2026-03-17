namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TypeCastTests() =

  let goodUpcastSpacingTest = """source :> target"""

  let badUpcastSpacingTest = """source:>target"""

  let goodDowncastSpacingTest = """source :?> target"""

  let badDowncastSpacingTest = """source:?>target"""

  [<TestMethod>]
  member _.``[TypeCast] Upcast Spacing Test``() =
    lint goodUpcastSpacingTest
    lintAssert badUpcastSpacingTest

  [<TestMethod>]
  member _.``[TypeCast] Downcast Spacing Test``() =
    lint goodDowncastSpacingTest
    lintAssert badDowncastSpacingTest