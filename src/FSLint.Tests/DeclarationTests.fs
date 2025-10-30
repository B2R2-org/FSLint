namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type DeclarationTests() =

  let goodTopBindingSpacingTest =
    """
let foo = 1

let bar = 2
"""

  let badTopBindingSpacingTest =
    """
let foo = 1
let bar = 2
"""

  let badTopBindingTooMuchSpacingTest =
    """
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

  [<TestMethod>]
  member _.``[Declaration] Single line fits in 80 columns``() =
    let code =
      """
let func = printfn "hello"
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[Declaration] Short function on one line``() =
    let code =
      """
let add x y = x + y
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[Declaration] Multi-line with long body - allowed``() =
    let code =
      """
let processData input =
  let step1 = transform input
  let step2 = validate step1
  compute step2
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[Declaration] Error - unnecessary line break``() =
    let code =
      """
let func =
  printfn "hello"
"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[Declaration] Error - short function unnecessarily split``() =
    let code =
      """
let add x y =
  x + y
"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[Declaration] Error - simple value unnecessarily split``() =
    let code =
      """
let value =
  42
"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore