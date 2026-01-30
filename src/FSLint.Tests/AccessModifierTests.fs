namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type AccessModifierTests() =

  [<TestMethod>]
  member _.``[AccessModifier] Error - private let in private module``() =
    let code =
      "module private MyModule\n" +
      "\n" +
      "let private func x = x + 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Good - public let in private module``() =
    let code =
      "module private MyModule\n" +
      "\n" +
      "let func x = x + 1\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Good - private let in public module``() =
    let code =
      "module MyModule\n" +
      "\n" +
      "let private func x = x + 1\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Error - private module in private module``() =
    let code =
      "module private Outer =\n" +
      "  module private Inner =\n" +
      "    let x = 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Good - public module in private module``() =
    let code =
      "module private Outer =\n" +
      "  module Inner =\n" +
      "    let x = 1\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Good - private module in public module``() =
    let code =
      "module Outer =\n" +
      "  module private Inner =\n" +
      "    let x = 1\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Error - private type in private module``() =
    let code =
      "module private MyModule\n" +
      "\n" +
      "type private MyType() =\n" +
      "  member _.Method() = ()\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Good - public type in private module``() =
    let code =
      "module private MyModule\n" +
      "\n" +
      "type MyType() =\n" +
      "  member _.Method() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Good - private type in public module``() =
    let code =
      "module MyModule\n" +
      "\n" +
      "type private MyType() =\n" +
      "  member _.Method() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Error - private member in private type``() =
    let code =
      "type private MyType() =\n" +
      "  member private _.Helper() = ()\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Good - public member in private type``() =
    let code =
      "type private MyType() =\n" +
      "  member _.Helper() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Good - private member in public type``() =
    let code =
      "type MyType() =\n" +
      "  member private _.Helper() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Error - deeply nested private modules``() =
    let code =
      "module private Outer =\n" +
      "  module Middle =\n" +
      "    module private Inner =\n" +
      "      let x = 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Good - mixed access in nested structure``() =
    let code =
      "module private Outer =\n" +
      "  module Middle =\n" +
      "    let x = 1\n" +
      "    type MyType() =\n" +
      "      member _.Method() = x\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Good - complex type with mixed members``() =
    let code =
      "type MyType() =\n" +
      "  let mutable state = 0\n" +
      "\n" +
      "  member private _.IncrementState() = state <- state + 1\n" +
      "\n" +
      "  member _.DoSomething() =\n" +
      "    this.IncrementState()\n" +
      "    printfn \"%d\" state\n"
    linterForFs.Lint(FakeFsPath, code)