namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type AccessModifierTests() =

  [<TestMethod>]
  member _.``[AccessModifier] Error - private let in private module``() =
    "module private MyModule\n" +
    "\n" +
    "let private func x = x + 1\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[AccessModifier] Good - public let in private module``() =
    "module private MyModule\n" +
    "\n" +
    "let func x = x + 1\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Good - private let in public module``() =
    "module MyModule\n" +
    "\n" +
    "let private func x = x + 1\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Error - private module in private module``() =
    "module private Outer =\n" +
    "  module private Inner =\n" +
    "    let x = 1\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[AccessModifier] Good - public module in private module``() =
    "module private Outer =\n" +
    "  module Inner =\n" +
    "    let x = 1\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Good - private module in public module``() =
    "module Outer =\n" +
    "  module private Inner =\n" +
    "    let x = 1\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Error - private type in private module``() =
    "module private MyModule\n" +
    "\n" +
    "type private MyType() =\n" +
    "  member _.Method() = ()\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[AccessModifier] Good - public type in private module``() =
    "module private MyModule\n" +
    "\n" +
    "type MyType() =\n" +
    "  member _.Method() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Good - private type in public module``() =
    "module MyModule\n" +
    "\n" +
    "type private MyType() =\n" +
    "  member _.Method() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Error - private member in private type``() =
    "type private MyType() =\n" +
    "  member private _.Helper() = ()\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[AccessModifier] Good - public member in private type``() =
    "type private MyType() =\n" +
    "  member _.Helper() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Good - private member in public type``() =
    "type MyType() =\n" +
    "  member private _.Helper() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Error - deeply nested private modules``() =
    "module private Outer =\n" +
    "  module Middle =\n" +
    "    module private Inner =\n" +
    "      let x = 1\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[AccessModifier] Good - mixed access in nested structure``() =
    "module private Outer =\n" +
    "  module Middle =\n" +
    "    let x = 1\n" +
    "    type MyType() =\n" +
    "      member _.Method() = x\n"
    |> lint

  [<TestMethod>]
  member _.``[AccessModifier] Good - complex type with mixed members``() =
    "type MyType() =\n" +
    "  let mutable state = 0\n" +
    "\n" +
    "  member private _.IncrementState() = state <- state + 1\n" +
    "\n" +
    "  member _.DoSomething() =\n" +
    "    this.IncrementState()\n" +
    "    printfn \"%d\" state\n"
    |> lint