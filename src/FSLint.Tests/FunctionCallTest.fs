namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type FunctionCallTests() =

  let goodNonCurriedFuncTest = """Func(p1, p2, p3)"""

  let badNonCurriedFuncBracketSpacingTest = """Func( p1, p2, p3 )"""

  let badNonCurriedFuncSpacingTest = """Func (p1, p2, p3)"""

  let goodCurriedFuncPascalCaseTest = """str.Replace()"""

  let badCurriedFuncPascalCaseTest = """str.Replace ()"""

  let goodCurriedFuncLowerCaseTest = """str.replace ()"""

  let badCurriedFuncLowerCaseTest = """str.replace()"""

  let goodCurriedFuncPascalCaseNestedTest = """str.Substring(1).TrimStart()"""

  let badCurriedFuncPascalCaseNestedTest = """str.Substring(1).TrimStart ()"""

  /// A lambda argument is an argument like any other, and the gaps of the list
  /// holding it either all carry a line break or none of them does. One given
  /// a line of its own leaves the short arguments below it no line to share.
  let goodParenNewLineTest =
    """
let good =
  CountBackForBand
    (fun src _ currentBand -> VisGraph.getLayer src = currentBand)
    bandIndexValue
    isReallyTrue
"""

  let goodParenNewLineTest2 =
    """
let good =
  countBackForBand
    (fun src _ currentBand -> VisGraph.getLayer src = currentBand)
    bandIndexValue
    isReallyTrue
"""

  let badParenNewLineTest =
    """
let good =
  CountBackForBand
    (fun src _ currentBand -> VisGraph.getLayer src = currentBand)
    bandIndexValue isReallyTrue
"""

  [<TestMethod>]
  member _.``[FunctionCall] Non Curried Function Bracket Spacing Test``() =
    lint goodNonCurriedFuncTest
    lintAssert badNonCurriedFuncBracketSpacingTest

  [<TestMethod>]
  member _.``[FunctionCall] Non Curried Function App Spacing Test``() =
    lintAssert badNonCurriedFuncSpacingTest

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function PascalCase Spacing Test``() =
    lint goodCurriedFuncPascalCaseTest
    lintAssert badCurriedFuncPascalCaseTest

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function LowerCase Spacing Test``() =
    lint goodCurriedFuncLowerCaseTest
    lintAssert badCurriedFuncLowerCaseTest

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function Nested Spacing Test``() =
    lint goodCurriedFuncPascalCaseNestedTest
    lintAssert badCurriedFuncPascalCaseNestedTest

  [<TestMethod>]
  member _.``[FunctionCall] Paren New Line Test``() =
    lint goodParenNewLineTest
    lint goodParenNewLineTest2