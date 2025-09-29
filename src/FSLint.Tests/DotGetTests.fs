namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type DotGetTests() =

  // ✅ OK: dot 앞뒤에 공백 없음 (괄호로 LHS를 고정해서 DotGet을 안정적으로 생성)
  let goodDotGet = """
module M =
  type C() =
    member _.Foo = 42

  let test (c: C) =
    let x = (c).Foo
    x
"""

  let badDotGetBefore = """
module M =
  type C() =
    member _.Foo = 42

  let test (c: C) =
    let x = (c) .Foo
    x
"""

  let badDotGetAfter = """
module M =
  type C() =
    member _.Foo = 42

  let test (c: C) =
    let x = (c). Foo
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
