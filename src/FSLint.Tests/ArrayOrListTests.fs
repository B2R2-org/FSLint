namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// In ArrayOrListConvention, tests unrelated to array/list differences use only
/// lists for clarity, as not all cases require checking both types.
/// An annotation fences nothing off: an element wearing one is read on the
/// same terms as any other, rather than falling past the checks entirely.
module AnnotatedElementSamples =

  let goodAnnotatedElementTest =
    """
let a = [ (expr: SynExpr).Range ]
"""

  let badAnnotatedElementTest =
    """
let b = [ (expr: SynExpr).Range ;(other: SynExpr).Range ]
"""

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

  let badSeparatorOnOpeningLineTest =
    """
[ 1;
  2
  3 ]
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

  let goodTypeAppInListTest = """[ typeof<int> ]"""

  let goodTypeAppInArrayTest = """[| typeof<int> |]"""

  let goodTypeAppMultiElementTest = """[ typeof<int>; typeof<string> ]"""

  let goodNestedTypeAppTest = """[ typeof<Dictionary<int, string>> ]"""

  let goodTypeAppApplicationTest = """[ Dictionary<int, string>() ]"""

  let goodTypeAppInTupleTest = """[ (typeof<int>, 1) ]"""

  let badTypeAppAngleSpacingTest = """[ typeof< int > ]"""

  let badNestedTypeAppCommaTest = """[ typeof<Dictionary<int,string>> ]"""

  let badTypeAppBracketSpacingTest = """[typeof<int>]"""

  let badTypeAppSeparatorSpacingTest = """[ typeof<int>;typeof<string> ]"""

  let badTypeAppTrailingSeparatorTest = """[ typeof<int>; ]"""

  let goodTypeAppOutsideTest = """typeof<int>"""

  let badTypeAppOutsideTest = """typeof< int >"""

  [<TestMethod>]
  member _.``[ArrayOrList] List Empty Test``() =
    lint goodEmptyTest
    lintAssert badEmptyTest

  [<TestMethod>]
  member _.``[ArrayOrList] Array Empty Test``() =
    lint goodArrayEmptyTest
    lintAssert badArrayEmptyTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing Test``() =
    lint goodBracketSpacingTest
    lintAssert badBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing Test``() =
    lint goodArrayBracketSpacingTest
    lintAssert badArrayBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] List No Whitespace Between Element Test``() =
    lint goodElementSpacingTest
    lintAssert badNoWhitespaceBetweenElementsTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Too Much Whitespace Between Element Test``() =
    lintAssert badTooMuchWhitespaceBetweenElementsTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Whitespace Before Separator Test``() =
    lintAssert badWhitespaceBeforeSeparatorTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator Test``() =
    lint goodRangeOperatorTest
    lintAssert badRangeOperatorTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Ident Test``() =
    lint goodRangeOperatorWithIdentTest
    lintAssert badRangeOperatorWithIdentTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step Test``() =
    lint goodRangeOperatorWithStepTest
    lintAssert badRangeOperatorWithStepTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Range Operator With Step And Ident Test``() =
    lint goodRangeOperatorWithStepAndIdentTest
    lintAssert badRangeOperatorWithStepAndIdentTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Bracket Spacing MultiLine Test``() =
    lint goodMultiLineBracketSpacingTest
    lintAssert badMultiLineBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Array Bracket Spacing MultiLine Test``() =
    lint goodArrayMultiLineBracketSpacingTest
    lintAssert badArrayMultiLineBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Opening Bracket Inline With Let MultiLine Test``() =
    lint goodOpeningBracketInlineWithLetTest
    lintAssert badOpeningBracketInlineWithLetTest

  [<TestMethod>]
  member _.``[ArrayOrList] List Single Element Per Line Multiline Test``() =
    lint goodSingleElementPerLineTest
    lintAssert badSingleElementPerLineTest

  /// Every line the literal spans is read but the last, the one the closing
  /// bracket sits on. The line it opens on is one of them: a separator left
  /// at the end of that line is the same mistake as one left further down,
  /// and reading from the line after it passed over the sample below.
  [<TestMethod>]
  member _.``[ArrayOrList] List Separator Not In Line Ending Test``() =
    lint goodSeparatorNotInLineEndingTest
    lintAssert badSeparatorNotInLineEndingTest
    lintAssert badSeparatorOnOpeningLineTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Bracket Spacing Test``() =
    lint goodNestedBracketSpacingTest
    lintAssert badNestedBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested List Element Spacing Test``() =
    lintAssert badNestedElementSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Bracket Spacing Test``() =
    lint goodNestedMixBracketSpacingTest
    lintAssert badNestedMixBracketSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Mixed Array List Element Spacing Test``() =
    lintAssert badNestedMixElementSpacingTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Bracket Spacing In MultiLine Test``() =
    lint goodNestedBracketSpacingMultiLineTest
    lintAssert badNestedBracketSpacingMultiLineTest

  [<TestMethod>]
  member _.``[ArrayOrList] Nested Element Spacing In MultiLine Test``() =
    lintAssert badNestedElementSpacingMultiLineTest

  [<TestMethod>]
  member _.``[ArrayOrList] TypeApp Element Test``() =
    lint goodTypeAppInListTest
    lint goodTypeAppInArrayTest
    lint goodTypeAppMultiElementTest
    lint goodNestedTypeAppTest
    lint goodTypeAppApplicationTest
    lint goodTypeAppInTupleTest

  [<TestMethod>]
  member _.``[ArrayOrList] TypeApp Element Spacing Test``() =
    lintAssert badTypeAppAngleSpacingTest
    lintAssert badNestedTypeAppCommaTest
    lintAssert badTypeAppBracketSpacingTest
    lintAssert badTypeAppSeparatorSpacingTest
    lintAssert badTypeAppTrailingSeparatorTest

  [<TestMethod>]
  member _.``[ArrayOrList] TypeApp Outside List Test``() =
    lint goodTypeAppOutsideTest
    lintAssert badTypeAppOutsideTest

  /// An element behind a type annotation is still an element, and the spacing
  /// beside it is still read.
  ///
  /// What this cannot pin is the other half of the case: without the `Typed`
  /// arm the walk falls into the TODO it keeps for shapes it does not know,
  /// and that note goes to the error stream, raising nothing. The test host
  /// holds that stream itself, so the note never reaches a test. Running the
  /// linter over `src/` and reading the output is what catches that.
  [<TestMethod>]
  member _.``[ArrayOrList] Annotated Element Test``() =
    lint AnnotatedElementSamples.goodAnnotatedElementTest
    lintAssertMsg "Remove whitespace before ';'"
      AnnotatedElementSamples.badAnnotatedElementTest
