namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

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
    lint goodElseExprExistTest
    lintAssert badElseExprExistTest

  [<TestMethod>]
  member _.``[IfThenElse] Else Expression not Exist Test(2)``() =
    lint goodElseExprExistTest2
    lintAssert badElseExprExistTest2

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test``() =
    lint goodKeywordSpacingTest
    lintAssert badKeywordSpacingTest

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(2)``() =
    lintAssert badKeywordSpacingTest2

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(3)``() =
    lintAssert badKeywordSpacingTest3