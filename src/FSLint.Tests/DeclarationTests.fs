namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type DeclarationTests() =

  let goodTopBindingSpacingTest = """
let foo = 1

let bar = 2
"""

  let badTopBindingSpacingTest = """
let foo = 1
let bar = 2
"""

  let badTopBindingTooMuchSpacingTest = """
let foo = 1


let bar = 2
"""

  [<TestMethod>]
  member _.``[ModuleDeclaration] Top Binding Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTopBindingSpacingTest
    assertFSLintFailure Constants.FakeFsPath badTopBindingSpacingTest

  [<TestMethod>]
  member _.``[ModuleDeclaration] Top Binding Too Much Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTopBindingSpacingTest
    assertFSLintFailure Constants.FakeFsPath badTopBindingTooMuchSpacingTest