namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

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
    linterForFs.Lint(FakeFsPath, goodCommaSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSpacingBeforeCommaTest)
     ) |> ignore
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badSpacingAfterCommaTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[Tuple] MultiLine Comma Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodCommaSpacingWithCommentTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBeforeCommaSpacingWithCommentTest)
     ) |> ignore
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badAfterCommaSpacingWithCommentTest)
     ) |> ignore

  [<TestMethod>]
  member _.``[Tuple] SingleLine Comma Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodCommaSpacingWithCommentTest2)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBeforeCommaSpacingWithCommentTest2)
     ) |> ignore
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badAfterCommaSpacingWithCommentTest2)
     ) |> ignore

  [<TestMethod>]
  member _.``[Tuple] Cons Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodConsSpacingWithCommentTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badConsSpacingWithCommentTest)
     ) |> ignore
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badConsSpacingWithCommentTest2)
     ) |> ignore

  [<TestMethod>]
  member _.``[Tuple] Pattern Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodCommaSpacingInPatternTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badCommaSpacingInPatternTest)
     ) |> ignore
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badCommaSpacingInPatternTest2)
     ) |> ignore