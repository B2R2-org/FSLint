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