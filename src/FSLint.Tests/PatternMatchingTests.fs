namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type PatternMatchingTests() =

  let goodPatternBracketSpacingTest =
    """
match good with
| [ 1; 2; 3 ] -> 1
| _ -> 2
"""

  let badPatternBracketSpacingTest =
    """
match bad with
| [1; 2; 3] -> 1
| _ -> 2
"""

  let badPatternElementSpacingTest =
    """
match bad with
| [1; 2;3 ] -> 1
| _ -> 2
"""

  let goodPatternConsOperatorTest =
    """
match good with
| x :: xs -> 1
| _ -> 2
"""

  let badPatternConsOperatorTest =
    """
match bad with
| x ::xs -> 1
| _ -> 2
"""

  let goodBarAndPatternIsInlineTest =
    """
match x with
| Foo -> Some good
| Bar -> None
"""

  let badBarAndPatternIsInlineTest =
    """
match x with
| Foo |
  Bar -> Some bad
"""

  let badBarAndMatchNotSameColTest =
    """
match x with
  | Foo
  | Bar -> Some good
"""

  let goodBarAndPatternSpacingTest =
    """
match x with
| Foo | Bar -> Some good
"""

  let badBarAndPatternSpacingTest =
    """
match x with
| Foo |Bar -> Some good
"""

  let badArrowSpacingTest =
    """
match x with
| Foo | Bar-> Some good
"""

  let badArrowSpacingWithWhenTest =
    """
match x with
| Foo | Bar when cond-> Some good
"""

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Bracket Spacing Test``() =
    lint goodPatternBracketSpacingTest
    lintAssert badPatternBracketSpacingTest

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Element Spacing Test``() =
    lintAssert badPatternElementSpacingTest

  [<TestMethod>]
  member _.``[PatternMatching] List In Pattern Cons Operator Test``() =
    lint goodPatternConsOperatorTest
    lintAssert badPatternConsOperatorTest

  [<TestMethod>]
  member _.``[PatternMatching] Pattern And Bar Is Not Inline Test``() =
    lint goodBarAndPatternIsInlineTest
    lintAssert badBarAndPatternIsInlineTest

  [<TestMethod>]
  member _.``[PatternMatching] Match Keyword and Bar Is Same Column Test``() =
    lintAssert badBarAndMatchNotSameColTest

  [<TestMethod>]
  member _.``[PatternMatching] Pattern and Bar Spacing Test``() =
    lint goodBarAndPatternSpacingTest
    lintAssert badBarAndPatternSpacingTest

  [<TestMethod>]
  member _.``[PatternMatching] Pattern Arrow Spacing Test``() =
    lintAssert badArrowSpacingTest
    lintAssert badArrowSpacingWithWhenTest