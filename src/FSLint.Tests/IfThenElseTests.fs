namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type IfThenElseTests() =

  let goodElseExprExistTest =
    """
    if foo then printfn "good" else printfn "good2"
"""

  let badElseExprExistTest =
    """
    if foo then printfn "bad"
"""

  let goodElseExprExistTest2 =
    """
    if foo then printfn "good"
    elif bar then printfn "good2"
    else printfn "good3"
"""

  let badElseExprExistTest2 =
    """
    if foo then printfn "bad"
    elif bar then printfn "bad2"
"""

  let goodKeywordSpacingTest =
    """
    if foo then printfn "good"
    elif bar then printfn "good2"
    else printfn "good3"
"""

  let badKeywordSpacingTest =
    """
    if foo then printfn "good"
    elif  bar then printfn "good2"
    else printfn "good3"
"""

  let badKeywordSpacingTest2 =
    """
    if foo  then printfn "good"
    elif bar then printfn "good2"
    else printfn "good3"
"""

  let badKeywordSpacingTest3 =
    """
    if foo then printfn "good"
    elif  bar then printfn "good2"
    else  printfn "good3"
"""

  let goodInlineElifChainTest =
    """
let fn v =
  if v = 0L then 1us elif v = 1L then 2us else 3us
"""

  let goodInlineIfElseTest =
    """
let fn v =
  if v = 0L then 1us else 2us
"""

  /// A chain too wide to close up stays broken, and is no more missing its
  /// else for having been broken.
  let goodWideElifChainTest =
    """
let fn (v: int64) =
  if v = 0L then 1us
  elif v >= -32768L && v <= 32767L && v <> 5L && v <> 7L then 2us
  else 3us
"""

  /// A comment between the links holds them apart, and that is still an else.
  let goodCommentedElifChainTest =
    """
let fn v =
  if v = 0L then 1us
  (* a note that cannot move *)
  elif v = 1L then 2us
  else 3us
"""

  /// A link holding neither keyword nor expression is the one truly missing
  /// its else, on one line as much as on several.
  let badInlineNoElseTest =
    """
let fn v =
  if v = 0L then printfn "zero"
"""

  let badInlineElifNoElseTest =
    """
let fn v =
  if v = 0L then printfn "zero" elif v = 1L then printfn "one"
"""

  /// Keyword spacing is still judged once the chain sits on one line.
  let badInlineElseSpacingTest =
    """
let fn v =
  if v = 0L then 1us elif v = 1L then 2us else  3us
"""

  let badInlineElifSpacingTest =
    """
let fn v =
  if v = 0L then 1us elif  v = 1L then 2us else 3us
"""

  let badInlineThenSpacingTest =
    """
let fn v =
  if v = 0L then 1us elif v = 1L then  2us else 3us
"""

  [<TestMethod>]
  member _.``[IfThenElse] Else Expression not Exist Test``() =
    lint goodElseExprExistTest
    lintAssert badElseExprExistTest

  [<TestMethod>]
  member _.``[IfThenElse] Else Expression not Exist Test(2)``() =
    lint goodElseExprExistTest2
    lintAssert badElseExprExistTest2

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test``() =
    lint goodKeywordSpacingTest
    lintAssert badKeywordSpacingTest

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(2)``() =
    lintAssert badKeywordSpacingTest2

  [<TestMethod>]
  member _.``[IfThenElse] Keyword Spacing Test(3)``() =
    lintAssert badKeywordSpacingTest3

  /// A chain laid on one line still has the else it was written with.
  [<TestMethod>]
  member _.``[IfThenElse] Inline Chain Has Else Test``() =
    lint goodInlineElifChainTest
    lint goodInlineIfElseTest

  /// Nor does the shape a chain is forced into change the answer.
  [<TestMethod>]
  member _.``[IfThenElse] Broken Chain Has Else Test``() =
    lint goodWideElifChainTest
    lint goodCommentedElifChainTest

  /// A missing else is still caught, on one line as much as on several.
  [<TestMethod>]
  member _.``[IfThenElse] Inline Missing Else Test``() =
    lintAssertMsg "Add else expression" badInlineNoElseTest
    lintAssertMsg "Add else expression" badInlineElifNoElseTest

  /// Keyword spacing is still judged once the chain sits on one line.
  [<TestMethod>]
  member _.``[IfThenElse] Inline Keyword Spacing Test``() =
    lintAssertMsg "Use single space after 'else'" badInlineElseSpacingTest
    lintAssertMsg "Use single whitespace after 'if'" badInlineElifSpacingTest
    lintAssertMsg "Use single whitespace after 'then'" badInlineThenSpacingTest

  /// The chain that started this: closing it up must not read as a missing
  /// else, or the two rules leave no shape that satisfies both.
  [<TestMethod>]
  member _.``[IfThenElse] Inline Chain No Rule Conflict Test``() =
    lintErrors goodInlineElifChainTest
    |> fun errors -> Assert.AreEqual<int>(0, errors.Length)