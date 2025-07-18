namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type FunctionCallTests () =

  let goodNonCurriedFuncTest = """Func (p1, p2, p3)"""

  let badNonCurriedFuncBracketSpacingTest = """Func ( p1, p2, p3 )"""

  let badNonCurriedFuncSpacingTest = """Func(p1, p2, p3)"""

  let goodCurriedFuncPascalCaseTest = """str.Replace()"""

  let badCurriedFuncPascalCaseTest = """str.Replace ()"""

  let goodCurriedFuncLowerCaseTest = """str.replace ()"""

  let badCurriedFuncLowerCaseTest = """str.replace()"""

  let goodCurriedFuncPascalCaseNestedTest = """str.Substring(1).TrimStart()"""

  let badCurriedFuncPascalCaseNestedTest = """str.Substring(1).TrimStart ()"""

  [<TestMethod>]
  member _.``[FunctionCall] Non Curried Function Bracket Spacing Test``() =
    linterForFs.Lint Constants.FakeFsPath goodNonCurriedFuncTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint Constants.FakeFsPath badNonCurriedFuncBracketSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[FunctionCall] Non Curried Function App Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint Constants.FakeFsPath badNonCurriedFuncSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function PascalCase Spacing Test``() =
    linterForFs.Lint Constants.FakeFsPath goodCurriedFuncPascalCaseTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint Constants.FakeFsPath badCurriedFuncPascalCaseTest
     ) |> ignore

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function LowerCase Spacing Test``() =
    linterForFs.Lint Constants.FakeFsPath goodCurriedFuncLowerCaseTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint Constants.FakeFsPath badCurriedFuncLowerCaseTest
     ) |> ignore

  [<TestMethod>]
  member _.``[FunctionCall] Curried Function Nested Spacing Test``() =
    linterForFs.Lint Constants.FakeFsPath goodCurriedFuncPascalCaseNestedTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint Constants.FakeFsPath badCurriedFuncPascalCaseNestedTest
     ) |> ignore