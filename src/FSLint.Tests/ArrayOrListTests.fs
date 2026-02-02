namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

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

  let goodRangeOperatorWithStepAndIdentTest =
    """
[ startIdent .. 2 .. endIdent ]
"""

  let badRangeOperatorWithStepAndIdentTest =
    """
[ startIdent..2 .. endIdent ]
"""

  let goodCommentPositionTest =
    """
[ 1; 2; 3 (* Good *) ]
"""

  let badCommentPositionTest =
    """
[ 1; 2; 3(* Bad *) ]
"""

  let goodMultiLineBracketSpacingTest =
    """
[ 1
  2
  3 ]
"""

  let badMultiLineBracketSpacingTest =
    """
[1
 2
 3 ]
"""

  let goodArrayMultiLineBracketSpacingTest =
    """
[| 1
   2
   3 |]
"""

  let badArrayMultiLineBracketSpacingTest =
    """
[|1
  2
  3 |]
"""

  let goodOpeningBracketInlineWithLetTest =
    """
let good =
 [ 1
   2
   3 ]
"""

  let badOpeningBracketInlineWithLetTest =
    """
let bad = [
  1
  2
  3
]
"""

  let goodSingleElementPerLineTest =
    """
[ 1
  2
  3
  4 ]
"""

  let badSingleElementPerLineTest =
    """
[ 1; 2
  3; 4 ]
"""

  let goodSeparatorNotInLineEndingTest =
    """
[ 1
  2
  3 ]
"""

  let badSeparatorNotInLineEndingTest =
    """
[ 1
  2;
  3 ]
"""

  let goodMultiLineCommentPositionTest =
    """
[ 1
  2
  3 (* Good *) ]
"""

  let badMultiLineCommentPositionTest =
    """
[ 1
  2
  3(* Bad *) ]
"""

  let goodNestedBracketSpacingTest =
    """
[ [ 1; 2 ]; [ 3; 4 ] ]
"""

  let goodNestedBracketSpacingMultiLineTest =
    """
[ [ 1; 2 ]
  [ 3; 4 ] ]
"""

  let badNestedBracketSpacingTest =
    """
[ [ 1; 2]; [ 3; 4 ] ]
"""

  let badNestedBracketSpacingMultiLineTest =
    """
[ [ 1; 2]
  [ 3; 4 ] ]
"""

  let badNestedElementSpacingTest =
    """
[ [ 1;2 ]; [ 3; 4 ] ]
"""

  let badNestedElementSpacingMultiLineTest =
    """
[ [ 1;2 ]
  [ 3; 4 ] ]
"""

  let goodNestedMixBracketSpacingTest =
    """
[ [| 1; 2 |]; [| 3; 4 |] ]
"""

  let badNestedMixBracketSpacingTest =
    """
[ [| 1; 2|]; [| 3; 4 |] ]
"""

  let badNestedMixElementSpacingTest =
    """
[ [| 1;2 |]; [| 3; 4 |] ]
"""

  [<TestMethod>]
  member _.``[ArrayOrList] List Empty Test``() =
    linterForFs.Lint(FakeFsPath, goodEmptyTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badEmptyTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Empty Test``() =
    linterForFs.Lint(FakeFsPath, goodArrayEmptyTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badArrayEmptyTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBracketSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodArrayBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badArrayBracketSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List No Whitespace Between Element Test``() =
    linterForFs.Lint(FakeFsPath, goodElementSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath,
        badNoWhitespaceBetweenElementsTest)
    ) |> ignore

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Too Much Whitespace Between Element Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath,
        badTooMuchWhitespaceBetweenElementsTest)
    ) |> ignore

  /// Normal case already covered in 'No Whitespace Between Element' test.
  [<TestMethod>]
  member _.``[ArrayOrList] List Whitespace Before Separator Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath,
        badWhitespaceBeforeSeparatorTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator Test``() =
    linterForFs.Lint(FakeFsPath, goodRangeOperatorTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badRangeOperatorTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Ident Test``() =
    linterForFs.Lint(FakeFsPath, goodRangeOperatorWithIdentTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badRangeOperatorWithIdentTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step Test``() =
    linterForFs.Lint(FakeFsPath, goodRangeOperatorWithStepTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badRangeOperatorWithStepTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step And Ident Test``() =
    linterForFs.Lint(FakeFsPath,
      goodRangeOperatorWithStepAndIdentTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath,
        badRangeOperatorWithStepAndIdentTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Comment Position Test``() =
    linterForFs.Lint(FakeFsPath, goodCommentPositionTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badCommentPositionTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing MultiLine Test``() =
    linterForFs.Lint(FakeFsPath, goodMultiLineBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badMultiLineBracketSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing MultiLine Test``() =
    linterForFs.Lint(FakeFsPath, goodArrayMultiLineBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath,
        badArrayMultiLineBracketSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Opening Bracket Inline With Let MultiLine Test``() =
    linterForFs.Lint(FakeFsPath, goodOpeningBracketInlineWithLetTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badOpeningBracketInlineWithLetTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Single Element Per Line Multiline Test``() =
    linterForFs.Lint(FakeFsPath, goodSingleElementPerLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSingleElementPerLineTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Separator Not In Line Ending Test``() =
    linterForFs.Lint(FakeFsPath, goodSeparatorNotInLineEndingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSeparatorNotInLineEndingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Comment Position MultiLine Test``() =
    linterForFs.Lint(FakeFsPath, goodMultiLineCommentPositionTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badMultiLineCommentPositionTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Bracket Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodNestedBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badNestedBracketSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Element Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badNestedElementSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Bracket Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodNestedMixBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badNestedMixBracketSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Element Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badNestedMixElementSpacingTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Bracket Spacing In MultiLine Test``() =
    linterForFs.Lint(FakeFsPath,
      goodNestedBracketSpacingMultiLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath,
        badNestedBracketSpacingMultiLineTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Element Spacing In MultiLine Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath,
        badNestedElementSpacingMultiLineTest)
     ) |> ignore