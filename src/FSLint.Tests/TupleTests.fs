namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// A parameter list and a tuple of data are not laid out the same way. The
/// first is a separator list like any other: while the whole of it would
/// close up onto one line it stays closed up, and once it would not, every
/// gap between neighbours has to agree. The second wants a name instead, so
/// that whatever holds it keeps its shape.
module TuplePlacementSamples =

  /// A struct tuple of data, spread because it does not fit.
  let badStructTupleTest =
    """
let fn () =
  struct (someRatherLongOperandName, anotherRatherLongOperandName,
          aThirdRatherLongOperandName)
"""

  /// Breaking at every comma is no answer either: it still does not fit.
  let badStructTupleBrokenTest =
    """
let fn () =
  struct (someRatherLongOperandName,
          anotherRatherLongOperandName,
          aThirdRatherLongOperandName)
"""

  let goodStructTupleTest =
    """
let fn () =
  printfn "anchor"
  struct (shortOne, shortTwo, shortThree)
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
  printfn "anchor"
  (shortOne, shortTwo, shortThree)
"""

  /// Named arguments are the same list, and mix the same way.
  let badNamedArgumentTest =
    """
let fn () =
  SomeConstructor(firstNamedArgument = someRatherLongValueHere,
                  secondNamedArgument = anotherRatherLongValue,
                  thirdNamedArgument = aThirdValue, fourth = more)
"""

  /// A row of a table is a tuple of data like any other, and answers the
  /// same way: the list holding it reads by the shape of its rows, and one
  /// spilling over its neighbours loses that shape.
  let badTableRowTest =
    """
let rows =
  [ "the first rather long field of this particular row of the table okay", 1,
    0UL
    "second row", 2, 1UL ]
"""

  /// Breaking the row at every comma is no answer: its commas then look like
  /// the separators of the list, and the rows can no longer be told apart.
  let badTableRowBrokenTest =
    """
let rows =
  [ "the first rather long field of this particular row of the table okay",
    1,
    0UL
    "second row", 2, 1UL ]
"""

  /// A row that does fit on one line is left alone.
  let goodTableRowTest =
    """
let rows =
  [ "first row", 1, 0UL
    "second row", 2, 1UL ]
"""

  /// The same tuple left on one line, where it runs past the line budget.
  /// Only the budget itself has anything to say about that.
  ///
  /// The sample is joined rather than quoted whole: the line it carries has to
  /// run past the budget to be worth testing, and written out here it would run
  /// past it in this file too.
  let badSingleLineTupleTest =
    "\nlet private formOf (probe: uint32) =\n  probe &&& 0x7Fu, "
    + "(probe >>> 12) &&& 0x7u, probe >>> 25, (probe >>> 20) &&& 0x1Fu\n"

  /// A tuple a function hands back has nowhere obvious to put a name, and is
  /// asked for one all the same: the caller reads it by its shape too.
  let badReturnTupleTest =
    """
let private formOf (probe: uint32) =
  probe &&& 0x7Fu, (probe >>> 12) &&& 0x7u, probe >>> 25,
  (probe >>> 20) &&& 0x1Fu
"""

  /// Two elements leave a single gap, which has nothing to disagree with.
  let goodTwoElementTupleTest =
    """
let fn () =
  someCall (someRatherLongArgumentNameThatGoesOnAndOnForQuiteAWhile,
            anotherRatherLongArgumentNameThatAlsoGoesOnForAWhileYet)
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

  /// A tuple of data too wide for its line wants a name, not a break: a list
  /// of such tuples reads as a table, and a row spread over its neighbours
  /// loses the shape the table is read by.
  [<TestMethod>]
  member _.``[Tuple] Element Placement Test``() =
    lint TuplePlacementSamples.goodStructTupleTest
    lint TuplePlacementSamples.goodPlainTupleTest
    lintAssertMsg "Bind to fit the line"
      TuplePlacementSamples.badStructTupleTest
    lintAssertMsg "Bind to fit the line"
      TuplePlacementSamples.badStructTupleBrokenTest
    lintAssertMsg "Bind to fit the line"
      TuplePlacementSamples.badPlainTupleTest

  /// Where the tuple stands makes no difference to any of this. A row of a
  /// table and a tuple handed back by a function are both tuples of data, and
  /// both want a name once they no longer fit.
  [<TestMethod>]
  member _.``[Tuple] Element Placement Test(3)``() =
    lint TuplePlacementSamples.goodTableRowTest
    lintAssertMsg "Bind to fit the line" TuplePlacementSamples.badTableRowTest
    lintAssertMsg "Bind to fit the line"
      TuplePlacementSamples.badTableRowBrokenTest
    lintAssertMsg "Bind to fit the line"
      TuplePlacementSamples.badReturnTupleTest

  /// A tuple still standing on one line has chosen no layout to answer for.
  /// What is wrong with it is the length of the line, and asking for a name
  /// on top of that says the same thing twice.
  [<TestMethod>]
  member _.``[Tuple] SingleLine Width Test``() =
    lintErrors TuplePlacementSamples.badSingleLineTupleTest
    |> fun errors ->
      Assert.AreEqual<int>(1, errors.Length)
      StringAssert.Contains(errors.Head.Message, "exceeds 80 characters")

  /// A parameter list is the other thing entirely: it breaks at every comma,
  /// so a mixture is what it answers for. A two-element tuple has a single
  /// gap that can never disagree with itself.
  [<TestMethod>]
  member _.``[Tuple] Element Placement Test(2)``() =
    lintAssertMsg "Use consistent line breaks"
      TuplePlacementSamples.badNamedArgumentTest
    lint TuplePlacementSamples.goodTwoElementTupleTest
