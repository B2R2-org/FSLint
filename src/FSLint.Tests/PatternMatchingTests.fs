namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type PatternMatchingTests () =

  let goodPatternBracketSpacingTest = """
match good with
| [ 1; 2; 3 ] -> 1
| _ -> 2
"""

  let badPatternBracketSpacingTest = """
match bad with
| [1; 2; 3] -> 1
| _ -> 2
"""

  let badPatternElementSpacingTest = """
match bad with
| [1; 2;3 ] -> 1
| _ -> 2
"""

  let goodPatternConsOperatorTest = """
match good with
| x :: xs -> 1
| _ -> 2
"""

  let badPatternConsOperatorTest = """
match bad with
| x ::xs -> 1
| _ -> 2
"""

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodPatternBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badPatternBracketSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Element Spacing Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badPatternElementSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Cons Operator Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodPatternConsOperatorTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badPatternConsOperatorTest
     ) |> ignore