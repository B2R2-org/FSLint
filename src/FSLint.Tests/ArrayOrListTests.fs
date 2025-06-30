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

  let goodListSpaceInfixTest = """
[ this.Address + uint64 this.Length + this.Name ]
"""

  let badListSpaceInfixTest = """
[ this.Address +  uint64 this.Length + this.Name ]
"""

  let goodListAppIndexerTest = """
[ lifter.File.RawBytes[ptr.Offset] ]
"""

  let badListAppIndexerTest = """
[ lifter.File.RawBytes[ ptr.Offset ] ]
"""

  let goodListAppIndexerInRangeTest = """
good[1..]
"""

  let badListAppIndexerInRangeTest = """
bad[ 1.. ]
"""

  let goodListSpaceFunAppTest = """[ fn 1 2 3 x ]"""

  let badListSpaceFunAppTest = """[ fn  1 2 3 x ]"""

  let goodPatternBracketSpacingTest = """
match good with
| [ 1; 2; 3 ] -> 1
| _ -> 2
"""

  let badPatternBracketSpacingTest = """
match bad with
| [1; 2; 3] -> 1
| _ -> 2
"""

  let badPatternElementSpacingTest = """
match bad with
| [1; 2;3 ] -> 1
| _ -> 2
"""

  let goodPatternConsOperatorTest = """
match good with
| x :: xs -> 1
| _ -> 2
"""

  let badPatternConsOperatorTest = """
match bad with
| x ::xs -> 1
| _ -> 2
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
  member _.``[ArrayOrList] List Space Before and After Infix Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListSpaceInfixTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListSpaceInfixTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List App Indexer Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListAppIndexerTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListAppIndexerTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List App Indexer Index Range Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListAppIndexerInRangeTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListAppIndexerInRangeTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List Space Fununction Application Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodListSpaceFunAppTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badListSpaceFunAppTest
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

  [<TestMethod>]
  member _.``[ArrayOrList] List In Pattern Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodPatternBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badPatternBracketSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List In Pattern Element Spacing Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badPatternElementSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] List In Pattern Cons Operator Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodPatternConsOperatorTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badPatternConsOperatorTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodNestedBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badNestedBracketSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Element Spacing Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badNestedElementSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Bracket Spacing Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodNestedMixBracketSpacingTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badNestedMixBracketSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Element Spacing Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badNestedMixElementSpacingTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Bracket Spacing In MultiLine Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodNestedBracketSpacingMultiLineTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badNestedBracketSpacingMultiLineTest
     ) |> ignore

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Element Spacing In MultiLine Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badNestedElementSpacingMultiLineTest
     ) |> ignore