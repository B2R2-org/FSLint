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
    linterForFs.Lint(Constants.FakeFsPath, goodElseExprExistTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badElseExprExistTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Else Expression not Exist Test(2)``() =
    linterForFs.Lint(Constants.FakeFsPath, goodElseExprExistTest2)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badElseExprExistTest2)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodKeywordSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badKeywordSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(2)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badKeywordSpacingTest2)
     ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(3)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badKeywordSpacingTest3)
     ) |> ignore