namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

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