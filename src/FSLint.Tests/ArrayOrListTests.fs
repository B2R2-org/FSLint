namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

/// In ArrayOrListConvention, tests unrelated to array/list differences use only
/// lists for clarity, as not all cases require checking both types.
[<TestClass>]
type ArrayOrListTests () =

  let goodListEmptyTest = """[]"""

  let badListEmptyTest = """[ ]"""

  let goodArrayEmptyTest = """[||]"""

  let badArrayEmptyTest = """[| |]"""

  let goodListBracketSpacingTest = """[ 1; 2; 3; 4 ]"""

  let badListBracketSpacingTest = """[1; 2; 3; 4]"""

  let goodArrayBracketSpacingTest = """[| 1; 2; 3; 4 |]"""

  let badArrayBracketSpacingTest = """[|1; 2; 3; 4|]"""

  let goodListElementSpacingTest = """[ 1; 2 ]"""

  let badListNoWhitespaceBetweenElementsTest = """[ 1;2 ]"""

  let badListTooMuchWhitespaceBetweenElementsTest = """[ 1;  2 ]"""

  let badListWhitespaceBeforeSeparatorTest = """[ 1 ;2 ]"""

  let goodListRangeOperatorTest = """[ 1 .. 10 ]"""

  let badListRangeOperatorTest = """[ 1..10 ]"""

  let goodListRangeOperatorWithIdentTest = """[ startIdent .. endIdent ]"""

  let badListRangeOperatorWithIdentTest = """[ startIdent..endIdent ]"""

  let goodListRangeOperatorWithStepTest = """[ 1 .. 2 .. 10 ]"""

  let badListRangeOperatorWithStepTest = """[ 1 ..2.. 10 ]"""

  let goodListRangeOperatorWithStepAndIdentTest = """
[ startIdent .. 2 .. endIdent ]
"""

  let badListRangeOperatorWithStepAndIdentTest = """
[ startIdent..2 .. endIdent ]
"""

  [<TestMethod>]
  member _.``[ArrayOrList] List Empty Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListEmptyTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListEmptyTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Empty Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodArrayEmptyTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badArrayEmptyTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListBracketSpacingTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodArrayBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badArrayBracketSpacingTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List No Whitespace Between Element Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListElementSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badListNoWhitespaceBetweenElementsTest
    ) |> ignore

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Too Much Whitespace Between Element Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badListTooMuchWhitespaceBetweenElementsTest
    ) |> ignore

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Whitespace Before Separator Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badListWhitespaceBeforeSeparatorTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListRangeOperatorTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListRangeOperatorTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Ident Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListRangeOperatorWithIdentTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListRangeOperatorWithIdentTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListRangeOperatorWithStepTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListRangeOperatorWithStepTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step And Ident Test`` () =
    linterForFs.Lint Constants.FakeFsPath
      goodListRangeOperatorWithStepAndIdentTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badListRangeOperatorWithStepAndIdentTest
    ) |> ignore