namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// The elements of a tuple are a separator list like any other: while the
/// whole of it would close up onto one line it has to stay closed up, and
/// once it would not, every gap between neighbours has to agree.
module TuplePlacementSamples =

  /// A struct tuple whose first gap stays on the line and whose second breaks.
  let badStructTupleTest =
    """
let fn () =
  struct (someRatherLongOperandName, anotherRatherLongOperandName,
          aThirdRatherLongOperandName)
"""

  let goodStructTupleTest =
    """
let fn () =
  struct (someRatherLongOperandName,
          anotherRatherLongOperandName,
          aThirdRatherLongOperandName)
"""

  /// A plain tuple answers exactly as the struct one does.
  let badPlainTupleTest =
    """
let fn () =
  (someRatherLongOperandName, anotherRatherLongOperandName,
   aThirdRatherLongOperandName)
"""

  let goodPlainTupleTest =
    """
let fn () =
  (someRatherLongOperandName,
   anotherRatherLongOperandName,
   aThirdRatherLongOperandName)
"""

  /// Named arguments are the same list, and mix the same way.
  let badNamedArgumentTest =
    """
let fn () =
  SomeConstructor(firstNamedArgument = someRatherLongValueHere,
                  secondNamedArgument = anotherRatherLongValue,
                  thirdNamedArgument = aThirdValue, fourth = more)
"""

  /// Two elements leave a single gap, which has nothing to disagree with.
  let goodTwoElementTupleTest =
    """
let fn () =
  (someRatherLongOperandNameThatGoesOnAndOnAndOnForQuiteAWhileHere,
   anotherRatherLongOperandNameThatAlsoGoesOnForQuiteAWhileYetHere)
"""

[<TestClass>]
type TupleTests() =

  let goodCommaSpacingTest = """[ 1, 2, 3 ]"""

  let badSpacingAfterCommaTest = """[ 1,2, 3 ]"""

  let badSpacingBeforeCommaTest = """[ 1 , 2, 3 ]"""

  let goodCommaSpacingWithCommentTest =
    """
let good =
  bar (42, (* comment *)
       24)
"""

  let badBeforeCommaSpacingWithCommentTest =
    """
let bad =
  bar (42 , (* comment *)
       24)
"""

  let badAfterCommaSpacingWithCommentTest =
    """
let bad =
  bar (42,  (* comment *)
       24)
"""

  let goodCommaSpacingWithCommentTest2 =
    """
let good =
  bar (42, (* comment *) 24)
"""

  let badBeforeCommaSpacingWithCommentTest2 =
    """
let bad =
  bar (42 , (* comment *) 24)
"""

  let badAfterCommaSpacingWithCommentTest2 =
    """
let bad =
  bar (42,  (* comment *) 24)
"""

  let goodConsSpacingWithCommentTest =
    """
let foo (x (* t2 *) :: xs, y) = x + y
"""

  let badConsSpacingWithCommentTest =
    """
let foo (x (* t2 *)  :: xs, y) = x + y
"""

  let badConsSpacingWithCommentTest2 =
    """
let foo = (1 ::  (* test *) [ 2 ], 3)
"""

  let goodCommaSpacingInPatternTest =
    """
match good with
| 1, (* test *) 2 :: [ 3 ] -> ()
"""

  let badCommaSpacingInPatternTest =
    """
match bad with
| 1,  (* test *) 2 :: [ 3 ] -> ()
"""

  let badCommaSpacingInPatternTest2 =
    """
match bad with
| 1     , (* test *) 2 :: [ 3 ] -> ()
"""

  [<TestMethod>]
  member _.``[Tuple] Comma Spacing Test``() =
    lint goodCommaSpacingTest
    lintAssert badSpacingBeforeCommaTest
    lintAssert badSpacingAfterCommaTest

  [<TestMethod>]
  member _.``[Tuple] MultiLine Comma Spacing Test``() =
    lint goodCommaSpacingWithCommentTest
    lintAssert badBeforeCommaSpacingWithCommentTest
    lintAssert badAfterCommaSpacingWithCommentTest

  [<TestMethod>]
  member _.``[Tuple] SingleLine Comma Spacing Test``() =
    lint goodCommaSpacingWithCommentTest2
    lintAssert badBeforeCommaSpacingWithCommentTest2
    lintAssert badAfterCommaSpacingWithCommentTest2

  [<TestMethod>]
  member _.``[Tuple] Cons Spacing Test``() =
    lint goodConsSpacingWithCommentTest
    lintAssert badConsSpacingWithCommentTest
    lintAssert badConsSpacingWithCommentTest2

  [<TestMethod>]
  member _.``[Tuple] Pattern Spacing Test``() =
    lint goodCommaSpacingInPatternTest
    lintAssert badCommaSpacingInPatternTest
    lintAssert badCommaSpacingInPatternTest2

  /// A tuple too wide to close up must break at every comma or none.
  [<TestMethod>]
  member _.``[Tuple] Element Placement Test``() =
    lint TuplePlacementSamples.goodStructTupleTest
    lint TuplePlacementSamples.goodPlainTupleTest
    lintAssertMsg "Use consistent line breaks"
      TuplePlacementSamples.badStructTupleTest
    lintAssertMsg "Use consistent line breaks"
      TuplePlacementSamples.badPlainTupleTest

  /// Named arguments form the same list, and a two-element tuple has a single
  /// gap that can never disagree with itself.
  [<TestMethod>]
  member _.``[Tuple] Element Placement Test(2)``() =
    lintAssertMsg "Use consistent line breaks"
      TuplePlacementSamples.badNamedArgumentTest
    lint TuplePlacementSamples.goodTwoElementTupleTest
