namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// In ArrayOrListConvention, tests unrelated to array/list differences use only
/// lists for clarity, as not all cases require checking both types.
[<TestClass>]
type ArrayOrListTests() =

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

  let goodMultiLineBracketSpacingTest = """
[ 1
  2
  3 ]
"""

  let badMultiLineBracketSpacingTest = """
[1
 2
 3 ]
"""

  let goodArrayMultiLineBracketSpacingTest = """
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

  let goodNestedBracketSpacingTest = """
[ [ 1; 2 ]; [ 3; 4 ] ]
"""

  let goodNestedBracketSpacingMultiLineTest = """
[ [ 1; 2 ]
  [ 3; 4 ] ]
"""

  let badNestedBracketSpacingTest = """
[ [ 1; 2]; [ 3; 4 ] ]
"""

  let badNestedBracketSpacingMultiLineTest = """
[ [ 1; 2]
  [ 3; 4 ] ]
"""

  let badNestedElementSpacingTest = """
[ [ 1;2 ]; [ 3; 4 ] ]
"""

  let badNestedElementSpacingMultiLineTest = """
[ [ 1;2 ]
  [ 3; 4 ] ]
"""

  let goodNestedMixBracketSpacingTest = """
[ [| 1; 2 |]; [| 3; 4 |] ]
"""

  let badNestedMixBracketSpacingTest = """
[ [| 1; 2|]; [| 3; 4 |] ]
"""

  let badNestedMixElementSpacingTest = """
[ [| 1;2 |]; [| 3; 4 |] ]
"""

  [<TestMethod>]
  member _.``[ArrayOrList] List Empty Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodEmptyTest
    assertFSLintFailure Constants.FakeFsPath badEmptyTest

  [<TestMethod>]
  member _.``[ArrayOrList] Array Empty Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodArrayEmptyTest
    assertFSLintFailure Constants.FakeFsPath badArrayEmptyTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodArrayBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badArrayBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] List No Whitespace Between Element Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodElementSpacingTest
    assertFSLintFailure Constants.FakeFsPath badNoWhitespaceBetweenElementsTest

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Too Much Whitespace Between Element Test``() =
    assertFSLintFailure Constants.FakeFsPath badTooMuchWhitespaceBetweenElementsTest

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Whitespace Before Separator Test``() =
    assertFSLintFailure Constants.FakeFsPath badWhitespaceBeforeSeparatorTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodRangeOperatorTest
    assertFSLintFailure Constants.FakeFsPath badRangeOperatorTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Ident Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodRangeOperatorWithIdentTest
    assertFSLintFailure Constants.FakeFsPath badRangeOperatorWithIdentTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodRangeOperatorWithStepTest
    assertFSLintFailure Constants.FakeFsPath badRangeOperatorWithStepTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step And Ident Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodRangeOperatorWithStepAndIdentTest
    assertFSLintFailure Constants.FakeFsPath badRangeOperatorWithStepAndIdentTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Comment Position Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodCommentPositionTest
    assertFSLintFailure Constants.FakeFsPath badCommentPositionTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing MultiLine Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodMultiLineBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badMultiLineBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing MultiLine Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodArrayMultiLineBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badArrayMultiLineBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Opening Bracket Inline With Let MultiLine Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodOpeningBracketInlineWithLetTest
    assertFSLintFailure Constants.FakeFsPath badOpeningBracketInlineWithLetTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Single Element Per Line Multiline Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodSingleElementPerLineTest
    assertFSLintFailure Constants.FakeFsPath badSingleElementPerLineTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Separator Not In Line Ending Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodSeparatorNotInLineEndingTest
    assertFSLintFailure Constants.FakeFsPath badSeparatorNotInLineEndingTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Comment Position MultiLine Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodMultiLineCommentPositionTest
    assertFSLintFailure Constants.FakeFsPath badMultiLineCommentPositionTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Bracket Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodNestedBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badNestedBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Element Spacing Test``() =
    assertFSLintFailure Constants.FakeFsPath badNestedElementSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Bracket Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodNestedMixBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badNestedMixBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Element Spacing Test``() =
    assertFSLintFailure Constants.FakeFsPath badNestedMixElementSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Bracket Spacing In MultiLine Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodNestedBracketSpacingMultiLineTest
    assertFSLintFailure Constants.FakeFsPath badNestedBracketSpacingMultiLineTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Element Spacing In MultiLine Test``() =
    assertFSLintFailure Constants.FakeFsPath badNestedElementSpacingMultiLineTest