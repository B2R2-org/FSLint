namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ParenTests() =

  let goodEmptyTest = """()"""

  let badEmptyTest = """( )"""

  let goodBracketSpacingTest = """(1, 2)"""

  let badBracketSpacingTest = """( 1, 2 )"""

  let badClosingSpacingTest = """(1, 2 )"""

  /// A struct tuple keeps its parentheses inside its own range rather than in
  /// a `SynExpr.Paren` of its own, so they have to be found before they can be
  /// judged.
  let goodStructFenceTest =
    """
let fn () = struct (1, 2)
"""

  let badStructFenceFrontTest =
    """
let fn () = struct ( 1, 2)
"""

  let badStructFenceBackTest =
    """
let fn () = struct (1, 2 )
"""

  let badStructPatternFenceTest =
    """
let fn v =
  match v with
  | struct (a, b ) -> a
"""

  /// A comment stands inside the fence without belonging to the expression,
  /// so one written flush against the parenthesis leaves no gap to report.
  let goodFenceCommentTest =
    """
let fn count = (count &&& 15 (* COUNT *)) * 8
"""

  let badFenceCommentTest =
    """
let fn count = (count &&& 15 (* COUNT *) ) * 8
"""

  let goodTraitCallTest =
    """
let inline callM (x: ^T) = (^T: (member M: int) x)
"""

  [<TestMethod>]
  member _.``[Paren] Paren Empty Test``() =
    lint goodEmptyTest
    lintAssert badEmptyTest

  [<TestMethod>]
  member _.``[Paren] Paren Bracket Spacing Test``() =
    lint goodBracketSpacingTest
    lintAssert badBracketSpacingTest

  /// A fence answers for both of its sides. The closing one went unasked
  /// whenever the expression opened on the line the '(' stands on, which is
  /// every fence written on a single line.
  [<TestMethod>]
  member _.``[Paren] Closing Paren Spacing Test``() =
    lintAssertMsg "Remove whitespace before ')'" badClosingSpacingTest

  [<TestMethod>]
  member _.``[Paren] Struct Tuple Fence Test``() =
    lint goodStructFenceTest
    lintAssertMsg "Remove whitespace after '('" badStructFenceFrontTest
    lintAssertMsg "Remove whitespace before ')'" badStructFenceBackTest
    lintAssertMsg "Remove whitespace before ')'" badStructPatternFenceTest

  [<TestMethod>]
  member _.``[Paren] Fence Comment Test``() =
    lint goodFenceCommentTest
    lintAssertMsg "Remove whitespace before ')'" badFenceCommentTest

  [<TestMethod>]
  member _.``[Paren] Trait Call No False Positive Test``() =
    lint goodTraitCallTest