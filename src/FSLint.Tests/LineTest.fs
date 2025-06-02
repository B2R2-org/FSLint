namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type LineTest () =

  let goodOnes = String.replicate 72 "1"

  let badOnes = String.replicate 73 "1"

  let goodLine80Test = $"""
let x = {goodOnes}
"""

  let badLine80Test = $"""
let x = {badOnes}
"""

  let goodTrailingWhiteSpaceTest = """
type A () =
  member _.X = 42
"""

  let space = " "

  let badTrailingWhiteSpaceTest = $"""
type A () ={space}
  member _.X = 42
"""

  [<TestMethod>]
  member _.``80 Line Test`` () =
    lintTextString Constants.FakeFsPath goodLine80Test
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString Constants.FakeFsPath badLine80Test
    ) |> ignore

  [<TestMethod>]
  member _.``Trailing Whitespace Test`` () =
    lintTextString
      Constants.FakeFsPath
      goodTrailingWhiteSpaceTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        Constants.FakeFsPath
        badTrailingWhiteSpaceTest
    ) |> ignore
