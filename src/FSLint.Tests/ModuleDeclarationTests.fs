namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type ModuleDeclarationTests() =
  let goodModuleOpenTest = """
open System.Runtime.CompilerServices
open B2R2
"""

  let badModuleOpenTest = """
open System.Runtime.CompilerServices

open B2R2
"""

  let goodAttributeExprTest = """
[<assembly: InternalsVisibleTo("Tests")>]
do ()
"""

  let badAttributeExprTest = """
[<assembly: InternalsVisibleTo("Tests")>]

do ()
"""

  let goodTopBindingSpacingTest = """
let foo = 1

let bar = 2
"""

  let badTopBindingSpacingTest = """
let foo = 1
let bar = 2
"""

  let goodModuleTypeSpacingTest = """
open B2R2

type A() =
  member _.X = 42
"""

  let badModuleTypeSpacingTest = """
open B2R2
type A() =
  member _.X = 42
"""

  [<TestMethod>]
  member _.``[ModuleDeclaration] Module Open Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodModuleOpenTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badModuleOpenTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ModuleDeclaration] Attributes Expr Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodAttributeExprTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badAttributeExprTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ModuleDeclaration] Top Binding Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTopBindingSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTopBindingSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ModuleDeclaration] Module Body Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodModuleTypeSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badModuleTypeSpacingTest)
     ) |> ignore