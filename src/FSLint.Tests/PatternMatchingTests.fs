namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// A comma list in a pattern is laid out as one in an expression is: its gaps
/// either all carry a line break or none of them does. What a pattern is never
/// asked is a name, since there is nowhere in a pattern to put one.
module PatternCommaSamples =

  /// Two of the three share a line while the third does not. The bodies are
  /// long enough that neither can come up beside its arrow, so nothing but
  /// the pattern's commas has anything to answer for.
  let badNestedPatternCommaTest =
    """
let fn x zero =
  match x with
  | TwoOperands(OprSIMD(ThreeRegs(Scalar(d1, _), Scalar(d2, _),
                                  Scalar(d3, _))), _) ->
    aRatherLongFunctionNameHere d1 d2 d3 |> anotherRatherLongFunctionName
  | _ ->
    aRatherLongFunctionNameHere zero zero zero |> anotherRatherLongFunction
"""

  /// Every comma broken.
  let goodBrokenPatternCommaTest =
    """
let fn x zero =
  match x with
  | TwoOperands(OprSIMD(ThreeRegs(Scalar(d1, _),
                                  Scalar(d2, _),
                                  Scalar(d3, _))), _) ->
    aRatherLongFunctionNameHere d1 d2 d3 |> anotherRatherLongFunctionName
  | _ ->
    aRatherLongFunctionNameHere zero zero zero |> anotherRatherLongFunction
"""

  /// All on one line.
  let goodInlinePatternCommaTest =
    """
let fn x =
  match x with
  | TwoOperands(OprSIMD(ThreeRegs(Scalar(d1, _), Scalar(d2, _)))) -> [ d1; d2 ]
  | _ -> []
"""

/// A `when` guard answers for the gaps between its operands and for nothing
/// else. Where the `when` sits, where the `&&` sits, and how far anything is
/// indented are all left to the author.
module GuardLayoutSamples =

  /// Two operands leave one gap, which cannot disagree with itself. The `&&`
  /// trails its line here rather than opening the next; that is not read.
  let goodTwoOperandGuardTest =
    """
let fn c bin =
  match c with
  | c when c &&& 0b100111000000000001u = 0b000010000000000001u &&
           extract c 11u 8u <> 0b0000u ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
  | _ ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
"""

  /// Three operands with the first gap closed and the second broken.
  let badMixedGuardTest =
    """
let fn c bin =
  match c with
  | c when c &&& 0b100111000000000001u = 0b0001u && extract c 11u 8u <> 0b0u
           && extract c 7u 4u <> 0b0001u ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
  | _ ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
"""

  /// The same three with every gap broken.
  let goodBrokenGuardTest =
    """
let fn c bin =
  match c with
  | c when c &&& 0b100111000000000001u = 0b0001u
           && extract c 11u 8u <> 0b0000u
           && extract c 7u 4u <> 0b0001u ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
  | _ ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
"""

  /// A parenthesised group running past its line. An `if` condition would be
  /// asked for a name here; a guard has nowhere to put one, so it is not.
  let goodParenGroupGuardTest =
    """
let fn c bin =
  match c with
  | c when someRatherLongConditionName c = 0b0001u
           && (anotherRatherLongConditionName c = 0b0010u
               || aThirdRatherLongConditionName c = 0b0100u) ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
  | _ ->
    aRatherLongFunctionNameHere c bin |> anotherRatherLongFunctionNameHere
"""

/// The cons operator reads the space beside it the way a keyword does, and a
/// run of comments in front of it belongs to the pattern on its left.
module ConsCommentRunSamples =

  let goodConsRunTest =
    """
let fn xs =
  match xs with
  | y (* a *) (* b *) :: _ -> y
  | _ -> 0
"""

  let badConsRunTest =
    """
let fn xs =
  match xs with
  | y (* a *) (* b *)  :: _ -> y
  | _ -> 0
"""

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

  /// A list buried inside a constructor is a list still, and answers for its
  /// gaps like any other. A pattern is never asked for a name: there is
  /// nowhere in one to put it.
  [<TestMethod>]
  member _.``[PatternMatching] Nested Comma Layout Test``() =
    lint PatternCommaSamples.goodBrokenPatternCommaTest
    lint PatternCommaSamples.goodInlinePatternCommaTest
    lintAssertMsg "Use consistent line breaks"
      PatternCommaSamples.badNestedPatternCommaTest

  /// The operands of a guard agree on their breaks, as those of an `if`
  /// condition do. Nothing else about a guard is read: not where `when` sits,
  /// not where `&&` sits, and not the name an `if` condition would be asked
  /// for once a parenthesised group ran past its line. There is nowhere in a
  /// match clause to put that name.
  [<TestMethod>]
  member _.``[PatternMatching] Guard Operand Layout Test``() =
    lint GuardLayoutSamples.goodTwoOperandGuardTest
    lint GuardLayoutSamples.goodBrokenGuardTest
    lint GuardLayoutSamples.goodParenGroupGuardTest
    lintAssertMsg "Use consistent line breaks"
      GuardLayoutSamples.badMixedGuardTest

  /// Reading only the first comment of the run leaves the operator looking a
  /// run's width from the pattern, and the clause is reported for a space its
  /// author never wrote.
  [<TestMethod>]
  member _.``[PatternMatching] Cons Comment Run Test``() =
    lint ConsCommentRunSamples.goodConsRunTest
    lintAssertMsg "Use single whitespace before ':'"
      ConsCommentRunSamples.badConsRunTest
