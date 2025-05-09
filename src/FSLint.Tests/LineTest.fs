namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type LineTest () =

  let goodLine80Test = """
let x = 111111111111111111111111111111111111111111111111111111111111111111111111
"""

  let badLine80Test = """
let x = 1111111111111111111111111111111111111111111111111111111111111111111111111
"""

  let goodTrailingWhiteSpaceTest = """
type A () =
  member _.X = 42
"""

  let badTrailingWhiteSpaceTest = """
type A () = 
  member _.X = 42
"""

  [<TestMethod>]
  member _.``80 Line Test`` () =
    lintTextString (nameof goodLine80Test) goodLine80Test
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString (nameof badLine80Test) badLine80Test
    ) |> ignore

  [<TestMethod>]
  member _.``Trailing Whitespace Test`` () =
    lintTextString
      (nameof goodTrailingWhiteSpaceTest)
      goodTrailingWhiteSpaceTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        (nameof badTrailingWhiteSpaceTest)
        badTrailingWhiteSpaceTest
    ) |> ignore
