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
    assertFSLintSuccess Constants.FakeFsPath goodUpcastSpacingTest
    assertFSLintFailure Constants.FakeFsPath badUpcastSpacingTest

  [<TestMethod>]
  member _.``[TypeCast] Downcast Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodDowncastSpacingTest
    assertFSLintFailure Constants.FakeFsPath badDowncastSpacingTest