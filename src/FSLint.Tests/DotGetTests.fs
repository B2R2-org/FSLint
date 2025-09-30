namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type DotGetTests() =

  let goodDotGet = """
type C() =
  member _.Foo = 42

let test (c: C) =
  let x = (c).Foo
  x
"""

  let badDotGetBefore = """
type C() =
  member _.Foo = 42

let test (c: C) =
  let x = (c) .Foo   // space before dot
  x
"""

  let badDotGetAfter = """
type C() =
  member _.Foo = 42

let test (c: C) =
  let x = (c). Foo   // space after dot
  x
"""

  [<TestMethod>]
  member _.``[DotGet] Allow no space around dot (paren lhs)``() =
    linterForFs.Lint(Constants.FakeFsPath, goodDotGet)

  [<TestMethod>]
  member _.``[DotGet] Error when space before dot (paren lhs)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badDotGetBefore)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error when space after dot (paren lhs)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badDotGetAfter)
    ) |> ignore
