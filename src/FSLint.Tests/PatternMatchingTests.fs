namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type PatternMatchingTests() =

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

  let goodBarAndPatternIsInlineTest = """
match x with
| Foo -> Some good
| Bar -> None
"""

  let badBarAndPatternIsInlineTest = """
match x with
| Foo |
  Bar -> Some bad
"""

  let badBarAndMatchNotSameColTest = """
match x with
  | Foo
  | Bar -> Some good
"""

  let goodBarAndPatternSpacingTest = """
match x with
| Foo | Bar -> Some good
"""

  let badBarAndPatternSpacingTest = """
match x with
| Foo |Bar -> Some good
"""

  let badArrowSpacingTest = """
match x with
| Foo | Bar-> Some good
"""

  let badArrowSpacingWithWhenTest = """
match x with
| Foo | Bar when cond-> Some good
"""

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Bracket Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodPatternBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badPatternBracketSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Element Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badPatternElementSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Cons Operator Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodPatternConsOperatorTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badPatternConsOperatorTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] Pattern And Bar Is Not Inline Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBarAndPatternIsInlineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBarAndPatternIsInlineTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] Match Keyword and Bar Is Same Column Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBarAndMatchNotSameColTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] Pattern and Bar Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBarAndPatternSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBarAndPatternSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[PatternMatching] Pattern Arrow Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badArrowSpacingTest)
     ) |> ignore
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badArrowSpacingWithWhenTest)
     ) |> ignore