namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

/// In ArrayOrListConvention, tests unrelated to array/list differences use only
/// lists for clarity, as not all cases require checking both types.
[<TestClass>]
type ArrayOrListTests () =

  let goodEmptyTest = """[]"""

  let badEmptyTest = """[ ]"""

  let goodArrayEmptyTest = """[||]"""

  let badArrayEmptyTest = """[| |]"""

  let goodBracketSpacingTest = """[ 1; 2; 3; 4 ]"""

  let badBracketSpacingTest = """[1; 2; 3; 4]"""

  let goodArrayBracketSpacingTest = """[| 1; 2; 3; 4 |]"""

  let badArrayBracketSpacingTest = """[|1; 2; 3; 4|]"""

  let goodElementSpacingTest = """[ 1; 2 ]"""

  let badNoWhitespaceBetweenElementsTest = """[ 1;2 ]"""

  let badTooMuchWhitespaceBetweenElementsTest = """[ 1;  2 ]"""

  let badWhitespaceBeforeSeparatorTest = """[ 1 ;2 ]"""

  let goodRangeOperatorTest = """[ 1 .. 10 ]"""

  let badRangeOperatorTest = """[ 1..10 ]"""

  let goodRangeOperatorWithIdentTest = """[ startIdent .. endIdent ]"""

  let badRangeOperatorWithIdentTest = """[ startIdent..endIdent ]"""

  let goodRangeOperatorWithStepTest = """[ 1 .. 2 .. 10 ]"""

  let badRangeOperatorWithStepTest = """[ 1 ..2.. 10 ]"""

  let goodRangeOperatorWithStepAndIdentTest = """
[ startIdent .. 2 .. endIdent ]
"""

  let badRangeOperatorWithStepAndIdentTest = """
[ startIdent..2 .. endIdent ]
"""

  let goodCommentPositionTest = """
[ 1; 2; 3 (* Good *) ]
"""

  let badCommentPositionTest = """
[ 1; 2; 3(* Bad *) ]
"""

  let goodMultiLineBracketSpacingTest ="""
[ 1
  2
  3 ]
"""

  let badMultiLineBracketSpacingTest = """
[1
 2
 3 ]
"""

  let goodArrayMultiLineBracketSpacingTest ="""
[| 1
   2
   3 |]
"""

  let badArrayMultiLineBracketSpacingTest = """
[|1
  2
  3 |]
"""

  let goodOpeningBracketInlineWithLetTest = """
let good =
 [ 1
   2
   3 ]
"""

  let badOpeningBracketInlineWithLetTest = """
let bad = [
  1
  2
  3
]
"""

  let goodSingleElementPerLineTest = """
[ 1
  2
  3
  4 ]
"""

  let badSingleElementPerLineTest = """
[ 1; 2
  3; 4 ]
"""

  let goodSeparatorNotInLineEndingTest = """
[ 1
  2
  3 ]
"""

  let badSeparatorNotInLineEndingTest = """
[ 1
  2;
  3 ]
"""

  let goodMultiLineCommentPositionTest = """
[ 1
  2
  3 (* Good *) ]
"""

  let badMultiLineCommentPositionTest = """
[ 1
  2
  3(* Bad *) ]
"""

  let goodArraySpaceOperatorsTest = """
[| this.Address + uint64 this.Length + this.Name |]
"""

  let badArraySpaceOperatorsTest = """
[| this.Address +  uint64 this.Length + this.Name |]
"""

  [<TestMethod>]
  member _.``[ArrayOrList] List Empty Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodEmptyTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badEmptyTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Empty Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodArrayEmptyTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badArrayEmptyTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badBracketSpacingTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodArrayBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badArrayBracketSpacingTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List No Whitespace Between Element Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodElementSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badNoWhitespaceBetweenElementsTest
    ) |> ignore

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Too Much Whitespace Between Element Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badTooMuchWhitespaceBetweenElementsTest
    ) |> ignore

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Whitespace Before Separator Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badWhitespaceBeforeSeparatorTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodRangeOperatorTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badRangeOperatorTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Ident Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodRangeOperatorWithIdentTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badRangeOperatorWithIdentTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodRangeOperatorWithStepTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badRangeOperatorWithStepTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step And Ident Test`` () =
    linterForFs.Lint Constants.FakeFsPath
      goodRangeOperatorWithStepAndIdentTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath
        badRangeOperatorWithStepAndIdentTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Comment Position Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodCommentPositionTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badCommentPositionTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing MultiLine Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodMultiLineBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badMultiLineBracketSpacingTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing MultiLine Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodArrayMultiLineBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badArrayMultiLineBracketSpacingTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Opening Bracket Inline With Let MultiLine Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodOpeningBracketInlineWithLetTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badOpeningBracketInlineWithLetTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Space Before and After Operators Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodArraySpaceOperatorsTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badArraySpaceOperatorsTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Single Element Per Line Multiline Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodSingleElementPerLineTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badSingleElementPerLineTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Separator Not In Line Ending Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodSeparatorNotInLineEndingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badSeparatorNotInLineEndingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Comment Position MultiLine Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodMultiLineCommentPositionTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badMultiLineCommentPositionTest
     ) |> ignore
