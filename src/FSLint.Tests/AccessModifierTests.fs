namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type AccessModifierTests() =

  [<TestMethod>]
  member _.``[AccessModifier] Public module with private binding - good``() =
    let code =
      "module PublicModule\n" +
      "\n" +
      "let private helperFunc x = x + 1\n" +
      "\n" +
      "let publicFunc x = helperFunc x\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Public type with private member - good``() =
    let code =
      "type MyType() =\n" +
      "  member private _.Helper() = ()\n" +
      "\n" +
      "  member _.Public() = ()\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] No access modifiers - good``() =
    let code =
      "module MyModule\n" +
      "\n" +
      "let func x = x\n" +
      "\n" +
      "type MyType() =\n" +
      "  member _.Method() = ()\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Public module with private nested - good``() =
    let code =
      "module PublicModule =\n" +
      "  module private PrivateNested =\n" +
      "    let x = 1\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Internal type with private member - good``() =
    let code =
      "type internal InternalType() =\n" +
      "  member private _.Helper() = ()\n" +
      "\n" +
      "  member _.Public() = ()\n"
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[AccessModifier] Error - private binding in private``() =
    let code =
      "module private MyModule\n" +
      "\n" +
      "let private helperFunc x = x + 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Error - private member in private type``() =
    let code =
      "type private MyType() =\n" +
      "  member private _.Helper() = ()\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Error - internal binding in internal module``() =
    let code =
      "module internal MyModule\n" +
      "\n" +
      "let internal helperFunc x = x + 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Error - private nested module in private``() =
    let code =
      "module private Outer =\n" +
      "  module private Inner =\n" +
      "    let x = 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Error - private type in private module``() =
    let code =
      "module private MyModule =\n" +
      "  type private MyType() =\n" +
      "    member _.Method() = ()\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Error - internal type in internal module``() =
    let code =
      "module internal MyModule =\n" +
      "  type internal MyType() =\n" +
      "    member _.Method() = ()\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Multiple bindings - mixed modifiers``() =
    let code =
      "module private MyModule\n" +
      "\n" +
      "let validFunc x = x\n" +
      "\n" +
      "let private redundantFunc x = x + 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Deeply nested modules``() =
    let code =
      "module private Outer =\n" +
      "  module Middle =\n" +
      "    module private Inner =\n" +
      "      let x = 1\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[AccessModifier] Complex type with mixed members - good``() =
    let code =
      "type MyType() =\n" +
      "  let mutable state = 0\n" +
      "\n" +
      "  member private _.IncrementState() = state <- state + 1\n" +
      "\n" +
      "  member _.DoSomething() =\n" +
      "    this.IncrementState()\n" +
      "    printfn \"%d\" state\n"
    linterForFs.Lint(Constants.FakeFsPath, code)