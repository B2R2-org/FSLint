namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type IfThenElseTests() =

  let goodElseExprExistTest =
    """
    if foo then printfn "good" else printfn "good2"
"""

  let badElseExprExistTest =
    """
    if foo then printfn "bad"
"""

  let goodElseExprExistTest2 =
    """
    if foo then printfn "good"
    elif bar then printfn "good2"
    else printfn "good3"
"""

  let badElseExprExistTest2 =
    """
    if foo then printfn "bad"
    elif bar then printfn "bad2"
"""

  let goodKeywordSpacingTest =
    """
    if foo then printfn "good"
    elif bar then printfn "good2"
    else printfn "good3"
"""

  let badKeywordSpacingTest =
    """
    if foo then printfn "good"
    elif  bar then printfn "good2"
    else printfn "good3"
"""

  let badKeywordSpacingTest2 =
    """
    if foo  then printfn "good"
    elif bar then printfn "good2"
    else printfn "good3"
"""

  let badKeywordSpacingTest3 =
    """
    if foo then printfn "good"
    elif  bar then printfn "good2"
    else  printfn "good3"
"""

  [<TestMethod>]
  member _.``[IfThenElse] Else Expression not Exist Test``() =
    linterForFs.Lint(FakeFsPath, goodElseExprExistTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badElseExprExistTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Else Expression not Exist Test(2)``() =
    linterForFs.Lint(FakeFsPath, goodElseExprExistTest2)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badElseExprExistTest2)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodKeywordSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badKeywordSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(2)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badKeywordSpacingTest2)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(3)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badKeywordSpacingTest3)
     ) |> ignore