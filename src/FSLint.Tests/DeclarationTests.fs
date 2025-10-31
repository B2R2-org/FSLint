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

  [<TestMethod>]
  member _.``[Declaration] Computation expression on next line - good``() =
    let code =
      "let loop () =\n" +
      "  async {\n" +
      "    return 42\n" +
      "  }\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[Declaration] Task expression on next line - good``() =
    let code =
      "let processData () =\n" +
      "  task {\n" +
      "    return! getData ()\n" +
      "  }\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[Declaration] Seq expression on next line - good``() =
    let code =
      "let numbers =\n" +
      "  seq {\n" +
      "    yield 1\n" +
      "    yield 2\n" +
      "  }\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[Declaration] Error - async on same line as equals``() =
    let code =
      "let loop () = async {\n" +
      "  return 42\n" +
      "}\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[Declaration] Error - task on same line as equals``() =
    let code =
      "let process () = task {\n" +
      "  return 1\n" +
      "}\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[Declaration] Error - seq on same line as equals``() =
    let code =
      "let nums = seq {\n" +
      "  yield 1\n" +
      "}\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[Declaration] Rec function with async on next line - good``() =
    let code =
      "let rec loop () =\n" +
      "  async {\n" +
      "    do! Async.Sleep 100\n" +
      "    return! loop ()\n" +
      "  }\n"
    linterForFs.Lint(Constants.FakeFsPath, code)