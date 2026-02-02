namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type FunctionBodyTests() =

  let goodEmptyNewLineTest =
    """
let fn () =
  let x = 1
  let y = 2
  x + y
"""

  let badEmptyNewLineTest =
    """
let fn () =
  let x = 1
  let y = 2

  x + y
"""

  let goodBindingWithAndKeywordTest =
    """
let fn () =
  let x = 1
  let y = 2
  x + y

and good = ()
"""

  let badBindingWithAndKeywordTest =
    """
let fn () =
  let x = 1
  let y = 2
  x + y
and bad = ()
"""

  [<TestMethod>]
  member _.``[FunctionBody] Empty NewLine Test``() =
    linterForFs.Lint(FakeFsPath, goodEmptyNewLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badEmptyNewLineTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[FunctionBody] Recursive Binding NewLine Test``() =
    linterForFs.Lint(FakeFsPath, goodBindingWithAndKeywordTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBindingWithAndKeywordTest)
     ) |> ignore