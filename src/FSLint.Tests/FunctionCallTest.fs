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
    assertFSLintSuccess Constants.FakeFsPath goodNonCurriedFuncTest
    assertFSLintFailure Constants.FakeFsPath badNonCurriedFuncBracketSpacingTest

  [<TestMethod>]
  member _.``[FunctionCall] Non Curried Function App Spacing Test``() =
    assertFSLintFailure Constants.FakeFsPath badNonCurriedFuncSpacingTest

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function PascalCase Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodCurriedFuncPascalCaseTest
    assertFSLintFailure Constants.FakeFsPath badCurriedFuncPascalCaseTest

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function LowerCase Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodCurriedFuncLowerCaseTest
    assertFSLintFailure Constants.FakeFsPath badCurriedFuncLowerCaseTest

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function Nested Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodCurriedFuncPascalCaseNestedTest
    assertFSLintFailure Constants.FakeFsPath badCurriedFuncPascalCaseNestedTest