namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

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
    linterForFs.Lint(Constants.FakeFsPath, goodTopBindingSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTopBindingSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ModuleDeclaration] Top Binding Too Much Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTopBindingTooMuchSpacingTest)
     ) |> ignore