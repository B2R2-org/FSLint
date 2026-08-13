namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// A `then` carrying a bare `if` is the one chain that cannot be closed up:
/// brought onto one line the inner `else` takes the one meant for the outer,
/// and F# refuses to read the result at all.
module BareInnerIfSamples =

  let goodBareInnerIfTest =
    """
let fn x i xs =
  if x.Min = i.Min then
    if x.Max = i.Max then true else xs
  else
    false
"""

  let badBareInnerIfTest =
    """
let fn x i xs =
  if x.Min = i.Min then
    if x.Max = i.Max then true else xs
  else false
"""

  /// Parenthesised, the inner `if` is an operand and the chain closes up.
  let badParenInnerIfTest =
    """
let fn x i xs =
  if x.Min = i.Min then
    (if x.Max = i.Max then true else xs)
  else false
"""

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
  printfn "checking"
  if v = 0L then 1us elif v = 1L then 2us else 3us
"""

  let goodInlineIfElseTest =
    """
let fn v =
  printfn "checking"
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
  printfn "checking"
  if v = 0L then printfn "zero"
"""

  let badInlineElifNoElseTest =
    """
let fn v =
  printfn "checking"
  if v = 0L then printfn "zero" elif v = 1L then printfn "one"
"""

  /// Keyword spacing is still judged once the chain sits on one line.
  let badInlineElseSpacingTest =
    """
let fn v =
  printfn "checking"
  if v = 0L then 1us elif v = 1L then 2us else  3us
"""

  let badInlineElifSpacingTest =
    """
let fn v =
  printfn "checking"
  if v = 0L then 1us elif  v = 1L then 2us else 3us
"""

  let badInlineThenSpacingTest =
    """
let fn v =
  printfn "checking"
  if v = 0L then 1us elif v = 1L then  2us else 3us
"""

  /// Neither spelling of a chain too wide to close up has anything to answer
  /// for, and the two must answer alike.
  let goodWideElifSpellingTest =
    """
let fn oldType newType =
  if oldType < newType then true
  elif oldType = newType then false
  else castErr newType oldType
"""

  let goodWideElseIfSpellingTest =
    """
let fn oldType newType =
  if oldType < newType then true
  else if oldType = newType then false
  else castErr newType oldType
"""

  /// A chain that does close up is still held to it, in either spelling.
  let badNarrowElifSpellingTest =
    """
let fn v =
  if v = 0 then 1
  elif v = 1 then 2
  else 3
"""

  let badNarrowElseIfSpellingTest =
    """
let fn v =
  if v = 0 then 1
  else if v = 1 then 2
  else 3
"""

  let goodNarrowInlineElseIfTest =
    """
let fn v = if v = 0 then 1 else if v = 1 then 2 else 3
"""

  /// A chain that closes up names the whole of what has to come up, not the
  /// last break alone: the body below its 'then' is as much in the wrong place
  /// as the 'else' hanging under it.
  let badClosableChainRangeTest =
    """
let fn count srcA srcB =
  if count < 64 then
      (srcB + srcA)
    else (srcB - srcA)
"""

  /// An 'else' handing its body a line of its own opens a nested expression
  /// rather than carrying the chain on, and the whole of it still closes up.
  let badNestedUnderElseTest =
    """
let fn v =
  if v = 0 then 1
  else
    if v = 1 then 2 else 3
"""

  /// The head breaks away and the tail shares a line, in either spelling.
  let badMixedLinksTailTest =
    """
let fn oldType newType =
  if oldType < newType then true
  else if oldType = newType then false else castErr newType oldType
"""

  let badMixedLinksTailElifTest =
    """
let fn oldType newType =
  if oldType < newType then true
  elif oldType = newType then false else castErr newType oldType
"""

  /// The same disagreement read the other way round.
  let badMixedLinksHeadTest =
    """
let fn oldType newType =
  if oldType < newType then true else if oldType = newType then false
  else castErr newType oldType
"""

  /// Two links leave a single gap, which has nothing to disagree with.
  let goodTwoLinkWideTest =
    """
let fn someLongName anotherLongName =
  if someLongName < anotherLongName then someLongName
  else castTypeError anotherLongName someLongName
"""

  /// A body broken onto its own line alongside links that disagree: the links
  /// are the outer question and answer for it alone.
  let badMixedLinksBrokenBodyTest =
    """
let fn oldType newType =
  if oldType < newType then
    true
  elif oldType = newType then false else castErr newType oldType
"""

  /// The condition runs over three lines; the bodies still fit beside their
  /// keywords, and so belong there.
  let goodBrokenConditionInlineBodyTest =
    """
let changeToAliasOfLDM bin =
  if (wbackW bin)
    && (pickFour bin 16 = 0b1101u)
    && (bitCount (extract bin 15 0) 15 > 1)
  then struct (Op.POP, OD.OprRegs)
  else struct (Op.LDM, OD.OprRnRegsA)
"""

  /// The same chain with bodies that could have sat beside their keywords but
  /// were sent below instead.
  let badBrokenConditionBelowBodyTest =
    """
let changeToAliasOfLDM bin =
  if (wbackW bin)
    && (pickFour bin 16 = 0b1101u)
    && (bitCount (extract bin 15 0) 15 > 1)
  then
    struct (Op.POP, OD.OprRegs)
  else
    struct (Op.LDM, OD.OprRnRegsA)
"""

  /// Parentheses fence off a group of their own, judged on the same terms.
  /// A group held on one line beside a broken outer chain is in order.
  let goodParenGroupInlineTest =
    """
let fn () =
  if isSomethingRatherLongHere
    && (isAnotherLongCondition || isYetAnotherLongCondition) then
    printfn "a message that is much too long to sit beside its keyword here"
  else
    printfn "another message far too long to sit beside its keyword as well"
"""

  /// The whole condition is one parenthesised group whose operands disagree.
  let badWholeParenGroupTest =
    """
let fn () =
  if (isSomethingRatherLongHere || isAnotherLongCondition
      || isYetAnotherLongCondition || isTheFourthLongCondition
      && isTheFifthLongCondition) then
    printfn "a message that is much too long to sit beside its keyword here"
  else
    printfn "another message far too long to sit beside its keyword as well"
"""

  /// A nested group whose operands disagree, though the outer chain agrees.
  let badNestedParenGroupTest =
    """
let fn () =
  if isSomethingRatherLongHere
    && (isAnotherLongCondition || isYetAnotherLongCondition
        || isTheFourthLongCondition) then
    printfn "a message that is much too long to sit beside its keyword here"
  else
    printfn "another message far too long to sit beside its keyword as well"
"""

  /// Breaking at every one of its gaps is no answer either: the group still
  /// does not keep to a line of its own, and still wants a name.
  let badNestedParenGroupBrokenTest =
    """
let fn () =
  if isSomethingRatherLongHere
    && (isAnotherLongCondition ||
        isYetAnotherLongCondition ||
        isTheFourthLongCondition) then
    printfn "a message that is much too long to sit beside its keyword here"
  else
    printfn "another message far too long to sit beside its keyword as well"
"""

  /// One body too wide to sit beside its keyword settles the whole chain on
  /// the broken layout, and the narrow one goes below with it.
  let goodOneWideBodyBelowTest =
    """
let changeToAliasOfLDM bin =
  if (wbackW bin)
    && (pickFour bin 16 = 0b1101u)
    && (bitCount (extract bin 15 0) 15 > 1)
  then
    struct (Op.LDM, OD.OprRnRegsWithRatherMoreBesidesThanCouldEverComeUpHereNow)
  else
    struct (Op.POP, OD.OprRegs)
"""

  /// The narrow one left beside its keyword while the wide one broke away.
  let badOneWideBodyMixedTest =
    """
let changeToAliasOfLDM bin =
  if (wbackW bin)
    && (pickFour bin 16 = 0b1101u)
    && (bitCount (extract bin 15 0) 15 > 1)
  then
    struct (Op.LDM, OD.OprRnRegsWithRatherMoreBesidesThanCouldEverComeUpHereNow)
  else struct (Op.POP, OD.OprRegs)
"""

  /// A body too wide to come up settles the chain on the broken layout, and a
  /// broken condition above it changes nothing about that.
  let goodBrokenConditionWideBodyTest =
    """
let changeToAliasOfLDM bin =
  if (wbackW bin)
    && (pickFour bin 16 = 0b1101u)
    && (bitCount (extract bin 15 0) 15 > 1)
  then
    struct (Op.POP, OD.OprRegsAndRatherMoreBesidesThatWillNotComeBackUpHereAt)
  else
    struct (Op.LDM, OD.OprRnRegsAndJustAsMuchAgainSoThatNeitherWillThisOneNow)
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

  /// A chain too wide to close up is left alone however its links are spelled.
  [<TestMethod>]
  member _.``[IfThenElse] Wide Chain Spelling Test``() =
    lint goodWideElifSpellingTest
    lint goodWideElseIfSpellingTest

  /// One that does close up is held to it however its links are spelled.
  [<TestMethod>]
  member _.``[IfThenElse] Narrow Chain Spelling Test``() =
    lintAssertMsg "Remove unnecessary line break" badNarrowElifSpellingTest
    lintAssertMsg "Remove unnecessary line break" badNarrowElseIfSpellingTest
    lint goodNarrowInlineElseIfTest

  /// Closing a chain up is one thing to do, so it takes one report, and the
  /// report covers everything standing below the line the chain opens on.
  [<TestMethod>]
  member _.``[IfThenElse] Closable Chain Range Test``() =
    lintErrors badClosableChainRangeTest
    |> fun errors ->
      Assert.AreEqual<int>(1, errors.Length)
      let range = errors.Head.Range
      (* from the body under 'then' through the end of the else body *)
      Assert.AreEqual<int>(4, range.StartLine)
      Assert.AreEqual<int>(6, range.StartColumn)
      Assert.AreEqual<int>(5, range.EndLine)
      Assert.AreEqual<int>(22, range.EndColumn)

  /// The two spellings must agree exactly, or the budget lands on one half of
  /// an 'else if' chain and tears it in two.
  [<TestMethod>]
  member _.``[IfThenElse] Chain Spellings Agree Test``() =
    let elifErrors = lintErrors goodWideElifSpellingTest |> List.length
    let elseIfErrors = lintErrors goodWideElseIfSpellingTest |> List.length
    Assert.AreEqual<int>(elifErrors, elseIfErrors)
    Assert.AreEqual<int>(0, elseIfErrors)

  /// An 'else' that hands its body a line of its own is a nested expression,
  /// not a chain link, and the whole of it still closes up.
  [<TestMethod>]
  member _.``[IfThenElse] Nested Under Else Test``() =
    lintAssertMsg "Remove unnecessary line break" badNestedUnderElseTest

  /// Links that disagree are reported though the chain is too wide to close
  /// up: closing up half of it is no more consistent than not closing it.
  [<TestMethod>]
  member _.``[IfThenElse] Mixed Links Test``() =
    lintAssertMsg "Use consistent line breaks" badMixedLinksTailTest
    lintAssertMsg "Use consistent line breaks" badMixedLinksTailElifTest
    lintAssertMsg "Use consistent line breaks" badMixedLinksHeadTest

  /// Both spellings must agree here too.
  [<TestMethod>]
  member _.``[IfThenElse] Mixed Links Spellings Agree Test``() =
    let elseIf = lintErrors badMixedLinksTailTest |> List.length
    let elifSpelling = lintErrors badMixedLinksTailElifTest |> List.length
    Assert.AreEqual<int>(elifSpelling, elseIf)
    Assert.AreEqual<int>(1, elseIf)

  /// A chain of two links has one gap, and one gap always agrees with itself.
  [<TestMethod>]
  member _.``[IfThenElse] Two Link Chain Test``() = lint goodTwoLinkWideTest

  /// The links settle the shape before the bodies do, so a chain wrong on
  /// both counts is reported once, not twice.
  [<TestMethod>]
  member _.``[IfThenElse] Mixed Links Single Report Test``() =
    lintErrors badMixedLinksBrokenBodyTest
    |> fun errors -> Assert.AreEqual<int>(1, errors.Length)

  /// A condition spread over several lines leaves the bodies to answer for
  /// their own widths, exactly as an unbroken one would.
  [<TestMethod>]
  member _.``[IfThenElse] Broken Condition Body Test``() =
    lint goodBrokenConditionInlineBodyTest
    lint goodBrokenConditionWideBodyTest
    lintAssertMsg "Remove unnecessary line break"
      badBrokenConditionBelowBodyTest

  /// One body that cannot come up takes the whole chain down with it, rather
  /// than leaving its narrow sibling beside a keyword on its own.
  [<TestMethod>]
  member _.``[IfThenElse] One Wide Body Takes The Chain Down``() =
    lint goodOneWideBodyBelowTest
    lintAssertMsg "Use consistent line breaks" badOneWideBodyMixedTest

  /// A parenthesised group is held to a line of its own. The chain around it
  /// may run down the page, breaking at every operator, but a group that
  /// cannot keep to one line is asked for a name instead.
  [<TestMethod>]
  member _.``[IfThenElse] Paren Condition Group Test``() =
    lint goodParenGroupInlineTest
    lintAssertMsg "Bind to fit the line" badWholeParenGroupTest
    lintAssertMsg "Bind to fit the line" badNestedParenGroupTest
    lintAssertMsg "Bind to fit the line"
      badNestedParenGroupBrokenTest

  /// Input the parser could make nothing of reaches the rules as an error
  /// node, and no rule may fall over on it.
  [<TestMethod>]
  member _.``[IfThenElse] Unparsable Chain Does Not Throw``() =
    let source =
      "let fn bin =\n" +
      "  if (wbackW bin)\n" +
      "    && (pickFour bin 16 = 0b1101u)\n" +
      "  then\n" +
      "  struct (Op.POP, OD.OprRegs)\n" +
      "  else\n" +
      "  struct (Op.LDM, OD.OprRnRegsA)\n"
    try
      lintErrors source |> ignore
    with ex ->
      (* A lint report is a fine outcome; falling through to the catch-all
         'TODO' of the expression walker is not. *)
      StringAssert.DoesNotMatch(ex.Message, System.Text.RegularExpressions
                                              .Regex "checkExpression TODO")

  /// A chain whose `then` carries a bare `if` can never be closed up: brought
  /// onto one line the inner `else` takes the one meant for the outer, and F#
  /// refuses to read the result at all. So the chain is not asked to close up;
  /// what it is asked is that its bodies agree.
  [<TestMethod>]
  member _.``[IfThenElse] Bare Inner If Test``() =
    lint BareInnerIfSamples.goodBareInnerIfTest
    lintAssertMsg "Use consistent line breaks"
      BareInnerIfSamples.badBareInnerIfTest

  /// Parenthesised, the inner `if` is an operand like any other and the chain
  /// closes up as it would have.
  [<TestMethod>]
  member _.``[IfThenElse] Paren Inner If Test``() =
    lintAssertMsg "Remove unnecessary line break"
      BareInnerIfSamples.badParenInnerIfTest
