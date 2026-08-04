namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// Sibling constructs must agree on their layout: either everything stays on
/// one line, or everything breaks onto a line of its own. Only the mixture is
/// reported. How long a line would have been never enters into it, so breaking
/// a short group out over several lines stays a free formatting choice.
[<TestClass>]
type LineBreakTests() =

  let goodBranchInlineTest =
    """
    if foo then printfn "good" else printfn "good2"
"""

  let goodBranchBrokenTest =
    """
    if foo then
      printfn "good"
    else
      printfn "good2"
"""

  let goodBranchElifTest =
    """
    if foo then
      printfn "good"
    elif bar then
      printfn "good2"
    else
      printfn "good3"
"""

  let badBranchTest =
    """
    if foo then printfn "bad"
    else
      printfn "bad2"
"""

  let badBranchElifTest =
    """
    if foo then printfn "bad"
    elif bar then
      printfn "bad2"
    else printfn "bad3"
"""

  let goodConditionInlineTest =
    """
    if foo && bar && baz then printfn "good" else printfn "good2"
"""

  let goodConditionBrokenTest =
    """
    if isSomethingRatherLongHere &&
       isAnotherRatherLongCondition &&
       isYetAnotherLongCondition then
      printfn "good"
    else
      printfn "good2"
"""

  /// Short enough to sit on one line, yet broken out anyway. The rule leaves
  /// that choice alone, because line length plays no part in it.
  let goodConditionShortTest =
    """
    if foo &&
       bar then
      printfn "good"
    else
      printfn "good2"
"""

  /// Mixing '&&' and '||' changes nothing: unparenthesised, the operands form
  /// one flat list, and every separator of it carries a break.
  let goodConditionMixedTest =
    """
    if isSomethingRatherLongHere &&
       isAnotherRatherLongCondition ||
       isYetAnotherLongCondition then
      printfn "good"
    else
      printfn "good2"
"""

  /// Parentheses mark a nested group, so the parenthesised operand is judged on
  /// its own and counts as a single operand of the outer '||'.
  let goodConditionParenTest =
    """
    if (isSomethingRatherLongHere && isAnotherRatherLongCondition) ||
       isYetAnotherLongCondition then
      printfn "good"
    else
      printfn "good2"
"""

  /// Without parentheses the '&&' separator is un-broken while the '||' one is
  /// broken. Operator precedence does not excuse the mixture, because the rule
  /// is about the layout of the text.
  let badConditionPrecedenceTest =
    """
    if isSomethingRatherLongHere && isAnotherRatherLongCondition ||
       isYetAnotherLongCondition then
      printfn "bad"
    else
      printfn "bad2"
"""

  let badConditionTest =
    """
    if isSomethingRatherLongHere && isAnotherRatherLongCondition &&
       isYetAnotherLongCondition then
      printfn "bad"
    else
      printfn "bad2"
"""

  let goodCaseInlineTest =
    """
    match value with
    | 1 -> printfn "good"
    | _ -> printfn "good2"
"""

  let goodCaseBrokenTest =
    """
    match value with
    | 1 ->
      printfn "good"
    | _ ->
      printfn "good2"
"""

  let badCaseTest =
    """
    match value with
    | 1 -> printfn "bad"
    | _ ->
      printfn "bad2"
"""

  /// `function` introduces the very same cases as `match`, so it is held to
  /// the same rule.
  let goodLambdaCaseTest =
    """
    let func = function
      | 1 ->
        printfn "good"
      | _ ->
        printfn "good2"
"""

  let badLambdaCaseTest =
    """
    let func = function
      | 1 ->
        printfn "bad"
      | _ -> printfn "bad2"
"""

  let goodCtorParamInlineTest =
    """
type TestClass(param1: int, param2: int, param3: int) =
  member _.Param1 = param1
"""

  let goodCtorParamBrokenTest =
    """
type TestClass(parameterNumberOne: int,
               parameterNumberTwo: int,
               parameterNumberThree: int) =
  member _.Param1 = parameterNumberOne
"""

  /// A short parameter list spread over several lines is likewise left alone.
  let goodCtorParamShortTest =
    """
type TestClass(param1: int,
               param2: int) =
  member _.Param1 = param1
"""

  let badCtorParamTest =
    """
type TestClass(parameterNumberOne: int, parameterNumberTwo: int,
               parameterNumberThree: int) =
  member _.Param1 = parameterNumberOne
"""

  /// A parameter may span several lines on its own; its own layout belongs to
  /// the nested group. Every separator of the outer list still carries a break.
  let goodMultiLineParamTest =
    """
type TestClass(lookup: Map<int,
                           string>,
               param2: int,
               param3: int) =
  member _.Param2 = param2
"""

  /// Only one separator here, so there is nothing for it to disagree with:
  /// 'param2' following 'lookup' on its closing line is simply un-broken.
  let goodSingleSeparatorTest =
    """
type TestClass(lookup: Map<int,
                           string>, param2: int) =
  member _.Param2 = param2
"""

  /// The first separator is un-broken while the second carries a break.
  let badMultiLineParamTest =
    """
type TestClass(lookup: Map<int,
                           string>, param2: int,
               param3: int) =
  member _.Param2 = param2
"""

  let goodFuncParamInlineTest =
    """
    let func (a: int) (b: int) (c: int) = a + b + c
"""

  let goodFuncParamBrokenTest =
    """
    let func (a: int)
             (b: int)
             (c: int) = a + b + c
"""

  let badFuncParamTest =
    """
    let func (parameterOne: int) (parameterTwo: int) (parameterThree: int)
             (parameterFour: int) = parameterOne
"""

  /// A type argument list is a group of its own, held to the same rule.
  let goodTypeArgTest =
    """
type TestClass(lookup: Map<int,
                           string>) =
  member _.Lookup = lookup
"""

  let badTypeArgTest =
    """
type TestClass(lookup: Map<int, string,
                           int>) =
  member _.Lookup = lookup
"""

  /// A chain of several 'elif' links is one group, so the whole chain has to
  /// agree.
  let goodLongElifTest =
    """
    if foo then
      printfn "good"
    elif bar then
      printfn "good2"
    elif baz then
      printfn "good3"
    else
      printfn "good4"
"""

  let badLongElifTest =
    """
    if foo then
      printfn "bad"
    elif bar then
      printfn "bad2"
    elif baz then printfn "bad3"
    else
      printfn "bad4"
"""

  /// The condition and the branch bodies are separate groups. A condition split
  /// over several lines does not oblige the bodies to break, nor the other way
  /// round.
  let goodBrokenConditionInlineBranchTest =
    """
    if isSomethingRatherLongHere &&
       isAnotherRatherLongCondition &&
       isYetAnotherLongCondition then printfn "good"
    else printfn "good2"
"""

  let goodInlineConditionBrokenBranchTest =
    """
    if foo && bar && baz then
      printfn "good"
    else
      printfn "good2"
"""

  /// The other way for a list to disagree: the first separator breaks and the
  /// second does not.
  let badConditionFirstBreakTest =
    """
    if isSomethingRatherLongHere &&
       isAnotherRatherLongCondition && isYetAnotherLongCondition then
      printfn "bad"
    else
      printfn "bad2"
"""

  let badCtorParamFirstBreakTest =
    """
type TestClass(param1: int,
               param2: int, param3: int) =
  member _.Param1 = param1
"""

  /// A guard sits between the pattern and the arrow, and the arrow is still
  /// what the body is measured against.
  let goodGuardedCaseTest =
    """
    match value with
    | n when n > 0 -> printfn "good"
    | _ -> printfn "good2"
"""

  let badGuardedCaseTest =
    """
    match value with
    | n when n > 0 -> printfn "bad"
    | _ ->
      printfn "bad2"
"""

  /// An or-pattern spans several lines but carries a single arrow, so it counts
  /// as one case.
  let goodOrPatternCaseTest =
    """
    match value with
    | 1
    | 2 -> printfn "good"
    | _ -> printfn "good2"
"""

  let badOrPatternCaseTest =
    """
    match value with
    | 1
    | 2 -> printfn "bad"
    | _ ->
      printfn "bad2"
"""

  /// A single case has nothing to disagree with.
  let goodSingleCaseTest =
    """
    match value with
    | _ ->
      printfn "good"
"""

  /// An inner match is a group of its own, judged apart from the outer one.
  let goodNestedCaseTest =
    """
    match value with
    | 1 ->
      match other with
      | 2 -> printfn "good"
      | _ -> printfn "good2"
    | _ ->
      printfn "good3"
"""

  let badNestedCaseTest =
    """
    match value with
    | 1 ->
      match other with
      | 2 -> printfn "bad"
      | _ ->
        printfn "bad2"
    | _ ->
      printfn "bad3"
"""

  /// 'match!' carries the same cases as 'match'.
  let goodMatchBangCaseTest =
    """
    let func () =
      async {
        match! foo () with
        | 1 ->
          printfn "good"
        | _ ->
          printfn "good2"
      }
"""

  let badMatchBangCaseTest =
    """
    let func () =
      async {
        match! foo () with
        | 1 -> printfn "bad"
        | _ ->
          printfn "bad2"
      }
"""

  let goodTupledParamTest =
    """
    let func (aaa: int,
              bbb: int,
              ccc: int) = aaa
"""

  let badTupledParamTest =
    """
    let func (aaa: int, bbb: int,
              ccc: int) = aaa
"""

  let goodMemberParamTest =
    """
type TestClass() =
  member _.Method(aaa: int,
                  bbb: int,
                  ccc: int) = aaa
"""

  let badMemberParamTest =
    """
type TestClass() =
  member _.Method(aaa: int, bbb: int,
                  ccc: int) = aaa
"""

  /// 'try' and its handler form a group of their own. The handler is measured
  /// from its '->', the same anchor a `match` case uses, so where the 'with'
  /// keyword sits does not enter into it.
  let goodTryWithInlineTest =
    """
    let func () =
      try foo ()
      with ex -> bar ()
"""

  let goodTryWithOneLineTest =
    """
    let func () = try foo () with ex -> bar ()
"""

  /// The 'with' keyword on a line of its own changes nothing: the handler body
  /// still sits beside its arrow, matching the inline 'try' body.
  let goodTryWithKeywordOwnLineTest =
    """
    let func () =
      try foo ()
      with
        ex -> bar ()
"""

  /// The 'try' body is broken out while the handler body stays beside its
  /// arrow.
  let badTryWithTest =
    """
    let func () =
      try
        foo ()
      with
        ex -> bar ()
"""

  let badTryWithReversedTest =
    """
    let func () =
      try
        foo ()
      with ex -> bar ()
"""

  /// The handler clauses are cases, held to the case rule as well.
  let goodTryWithClauseTest =
    """
    let func () =
      try
        foo ()
      with
      | ex ->
        bar ()
      | ex2 ->
        baz ()
"""

  let badTryWithClauseTest =
    """
    let func () =
      try
        foo ()
      with
      | ex -> bar ()
      | ex2 ->
        baz ()
"""

  /// The handler body broken out below its arrow, matching the broken 'try'
  /// body. This is the canonical multi-line shape.
  let goodTryWithPatternOnKeywordLineTest =
    """
    let func () =
      try
        foo ()
      with ex ->
        ()
"""

  let goodTryWithTypedHandlerTest =
    """
    let func () =
      try
        foo ()
      with :? System.IO.IOException as ex ->
        ()
"""

  /// 'try' pairs with 'finally' exactly as it does with 'with'.
  let goodTryFinallyInlineTest =
    """
    let func () =
      try foo ()
      finally bar ()
"""

  let goodTryFinallyBrokenTest =
    """
    let func () =
      try
        foo ()
      finally
        bar ()
"""

  let badTryFinallyTest =
    """
    let func () =
      try foo ()
      finally
        bar ()
"""

  let badTryFinallyReversedTest =
    """
    let func () =
      try
        foo ()
      finally bar ()
"""

  /// A type parameter list is comma separated too, so the same rule applies.
  let goodTypeParamInlineTest =
    """
type TestClass<'a, 'b, 'c>() =
  member _.Value = 1
"""

  let goodTypeParamBrokenTest =
    """
type TestClass<'a,
               'b,
               'c>() =
  member _.Value = 1
"""

  let badTypeParamTest =
    """
type TestClass<'a, 'b,
               'c>() =
  member _.Value = 1
"""

  /// An access modifier does not change how the parameters are judged.
  let goodPrivateCtorParamTest =
    """
type TestClass private(aaa: int,
                       bbb: int,
                       ccc: int) =
  member _.Aaa = aaa
"""

  let badPrivateCtorParamTest =
    """
type TestClass private(aaa: int, bbb: int,
                       ccc: int) =
  member _.Aaa = aaa
"""

  let badStaticMemberParamTest =
    """
type TestClass() =
  static member Method(aaa: int, bbb: int,
                       ccc: int) = aaa
"""

  /// Type arguments are judged wherever they appear, not only in a parameter.
  let badReturnTypeArgTest =
    """
    let func (): Map<int, string,
                     int> = Map.empty
"""

  let badRecordFieldTypeArgTest =
    """
type Record =
  { Field: Map<int, string,
               int> }
"""

  /// The outer list keeps both arguments together while the inner one breaks
  /// its own separator: each group is uniform on its own terms.
  let goodNestedTypeArgTest =
    """
    let func (x: Map<int, List<string,
                              int>>) = x
"""

  /// Only '||' in the chain, and only the second separator breaks.
  let badOrOnlyConditionTest =
    """
    if aLongConditionOne || aLongConditionTwo ||
       aLongConditionThree then
      printfn "bad"
    else
      printfn "bad2"
"""

  /// Each 'elif' link carries a condition group of its own.
  let badElifConditionTest =
    """
    if foo then
      printfn "bad"
    elif bLongConditionOne && bLongConditionTwo &&
         bLongConditionThree then
      printfn "bad2"
    else
      printfn "bad3"
"""

  /// Four operands with the disagreement in the middle.
  let badMiddleConditionTest =
    """
    if aLongConditionOne &&
       aLongConditionTwo && aLongConditionThree &&
       aLongConditionFour then
      printfn "bad"
    else
      printfn "bad2"
"""

  /// An operand may span lines of its own; the outer separators still have to
  /// agree.
  let badMultiLineOperandTest =
    """
    if someFunction (aaa,
                     bbb) && anotherCondition &&
       thirdCondition then
      printfn "bad"
    else
      printfn "bad2"
"""

  /// Three cases with the disagreement in the middle.
  let badMiddleCaseTest =
    """
    match value with
    | 1 ->
      printfn "bad"
    | 2 -> printfn "bad2"
    | _ ->
      printfn "bad3"
"""

  /// A condition that is not an operator chain at all has nothing to compare.
  let goodSingleOperandConditionTest =
    """
    if singleCondition then
      printfn "good"
    else
      printfn "good2"
"""

  let goodSingleTypeArgTest =
    """
    let func (x: Option<
                   int>) = x
"""

  let goodSingleCtorParamTest =
    """
type TestClass(aaa: int) =
  member _.Aaa = aaa
"""

  /// A constraint rides along on the last type parameter without upsetting the
  /// separators.
  let goodConstrainedTypeParamTest =
    """
type TestClass<'a,
               'b when 'b: comparison>() =
  member _.Value = 1
"""

  /// The try/with pairing and the handler-clause consistency are two separate
  /// checks: here the pairing is fine and only the clauses disagree.
  let badTryWithClauseOnlyTest =
    """
    let func () =
      try foo ()
      with
      | ex -> bar ()
      | ex2 ->
        baz ()
"""

  let goodGuardedHandlerTest =
    """
    let func () =
      try
        foo ()
      with ex when ex <> null ->
        bar ()
"""

  let badGuardedHandlerTest =
    """
    let func () =
      try
        foo ()
      with ex when ex <> null -> bar ()
"""

  let goodNestedTryWithTest =
    """
    let func () =
      try
        try
          inner ()
        with e1 ->
          ()
      with e2 ->
        ()
"""

  let badGuardedLambdaCaseTest =
    """
    let func = function
      | n when n > 0 -> printfn "bad"
      | _ ->
        printfn "bad2"
"""

  let goodOrPatternLambdaCaseTest =
    """
    let func = function
      | 1
      | 2 -> printfn "good"
      | _ -> printfn "good2"
"""

  /// A 'match' nested inside a 'try' is a group of its own, and so is a 'try'
  /// nested inside a case.
  let goodMatchInTryTest =
    """
    let func x =
      try
        match x with
        | 1 -> printfn "good"
        | _ -> printfn "good2"
      with e ->
        ()
"""

  let badTryInCaseTest =
    """
    let func x =
      match x with
      | 1 -> printfn "bad"
      | _ ->
        try
          foo ()
        with e ->
          ()
"""

  let badLambdaBodyCaseTest =
    """
    let func xs =
      xs |> List.map (fun x ->
        match x with
        | 1 -> "bad"
        | _ ->
          "bad2")
"""

  /// The disagreeing link sits in the middle of the chain.
  let badMiddleElifTest =
    """
    if foo then
      printfn "bad"
    elif bar then printfn "bad2"
    elif baz then
      printfn "bad3"
    else
      printfn "bad4"
"""

  /// Three type arguments, every separator broken.
  let goodLongTypeArgTest =
    """
type TestClass(lookup: Map<int,
                           string,
                           int>) =
  member _.Lookup = lookup
"""

  [<TestMethod>]
  member _.``[LineBreak] Branch Line Break Test``() =
    lint goodBranchInlineTest
    lint goodBranchBrokenTest
    lintAssert badBranchTest

  [<TestMethod>]
  member _.``[LineBreak] Branch Line Break Test(2)``() =
    lint goodBranchElifTest
    lintAssert badBranchElifTest

  [<TestMethod>]
  member _.``[LineBreak] Branch Line Break Test(3)``() =
    lint goodLongElifTest
    lintAssert badLongElifTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test``() =
    lint goodConditionInlineTest
    lint goodConditionBrokenTest
    lint goodConditionShortTest
    lintAssert badConditionTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test(2)``() =
    lint goodConditionMixedTest
    lint goodConditionParenTest
    lintAssert badConditionPrecedenceTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test(3)``() =
    lintAssert badConditionFirstBreakTest

  /// The condition group and the branch group are judged independently.
  [<TestMethod>]
  member _.``[LineBreak] Group Independence Test``() =
    lint goodBrokenConditionInlineBranchTest
    lint goodInlineConditionBrokenBranchTest

  [<TestMethod>]
  member _.``[LineBreak] Case Line Break Test``() =
    lint goodCaseInlineTest
    lint goodCaseBrokenTest
    lint goodSingleCaseTest
    lintAssert badCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Case Line Break Test(2)``() =
    lint goodLambdaCaseTest
    lintAssert badLambdaCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Case Line Break Test(3)``() =
    lint goodMatchBangCaseTest
    lintAssert badMatchBangCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Guarded Case Line Break Test``() =
    lint goodGuardedCaseTest
    lintAssert badGuardedCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Or Pattern Case Line Break Test``() =
    lint goodOrPatternCaseTest
    lintAssert badOrPatternCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Nested Case Line Break Test``() =
    lint goodNestedCaseTest
    lintAssert badNestedCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Parameter Line Break Test``() =
    lint goodCtorParamInlineTest
    lint goodCtorParamBrokenTest
    lint goodCtorParamShortTest
    lintAssert badCtorParamTest

  [<TestMethod>]
  member _.``[LineBreak] Parameter Line Break Test(2)``() =
    lint goodMultiLineParamTest
    lint goodSingleSeparatorTest
    lintAssert badMultiLineParamTest

  [<TestMethod>]
  member _.``[LineBreak] Parameter Line Break Test(3)``() =
    lint goodFuncParamInlineTest
    lint goodFuncParamBrokenTest
    lintAssert badFuncParamTest

  [<TestMethod>]
  member _.``[LineBreak] Parameter Line Break Test(4)``() =
    lintAssert badCtorParamFirstBreakTest

  [<TestMethod>]
  member _.``[LineBreak] Tupled Parameter Line Break Test``() =
    lint goodTupledParamTest
    lintAssert badTupledParamTest

  [<TestMethod>]
  member _.``[LineBreak] Member Parameter Line Break Test``() =
    lint goodMemberParamTest
    lintAssert badMemberParamTest

  [<TestMethod>]
  member _.``[LineBreak] Try With Line Break Test``() =
    lint goodTryWithInlineTest
    lint goodTryWithOneLineTest
    lint goodTryWithKeywordOwnLineTest
    lintAssert badTryWithTest

  [<TestMethod>]
  member _.``[LineBreak] Try With Line Break Test(2)``() =
    lintAssert badTryWithReversedTest

  [<TestMethod>]
  member _.``[LineBreak] Try With Line Break Test(3)``() =
    lint goodTryWithPatternOnKeywordLineTest
    lint goodTryWithTypedHandlerTest

  [<TestMethod>]
  member _.``[LineBreak] Try With Clause Line Break Test``() =
    lint goodTryWithClauseTest
    lintAssert badTryWithClauseTest

  [<TestMethod>]
  member _.``[LineBreak] Try Finally Line Break Test``() =
    lint goodTryFinallyInlineTest
    lint goodTryFinallyBrokenTest
    lintAssert badTryFinallyTest

  [<TestMethod>]
  member _.``[LineBreak] Try Finally Line Break Test(2)``() =
    lintAssert badTryFinallyReversedTest

  [<TestMethod>]
  member _.``[LineBreak] Type Parameter Line Break Test``() =
    lint goodTypeParamInlineTest
    lint goodTypeParamBrokenTest
    lintAssert badTypeParamTest

  [<TestMethod>]
  member _.``[LineBreak] Private Constructor Parameter Line Break Test``() =
    lint goodPrivateCtorParamTest
    lintAssert badPrivateCtorParamTest

  [<TestMethod>]
  member _.``[LineBreak] Static Member Parameter Line Break Test``() =
    lintAssert badStaticMemberParamTest

  [<TestMethod>]
  member _.``[LineBreak] Type Argument Line Break Test(2)``() =
    lint goodNestedTypeArgTest
    lintAssert badReturnTypeArgTest
    lintAssert badRecordFieldTypeArgTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test(4)``() =
    lintAssert badOrOnlyConditionTest
    lintAssert badElifConditionTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test(5)``() =
    lintAssert badMiddleConditionTest
    lintAssert badMultiLineOperandTest

  [<TestMethod>]
  member _.``[LineBreak] Case Line Break Test(4)``() =
    lintAssert badMiddleCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Case Line Break Test(5)``() =
    lint goodOrPatternLambdaCaseTest
    lintAssert badGuardedLambdaCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Branch Line Break Test(4)``() =
    lintAssert badMiddleElifTest

  /// A group with a single item has nothing to disagree with.
  [<TestMethod>]
  member _.``[LineBreak] Single Item Group Test``() =
    lint goodSingleOperandConditionTest
    lint goodSingleTypeArgTest
    lint goodSingleCtorParamTest

  [<TestMethod>]
  member _.``[LineBreak] Type Parameter Line Break Test(2)``() =
    lint goodConstrainedTypeParamTest

  [<TestMethod>]
  member _.``[LineBreak] Try With Clause Line Break Test(2)``() =
    lintAssert badTryWithClauseOnlyTest

  [<TestMethod>]
  member _.``[LineBreak] Guarded Handler Line Break Test``() =
    lint goodGuardedHandlerTest
    lintAssert badGuardedHandlerTest

  [<TestMethod>]
  member _.``[LineBreak] Nested Try With Line Break Test``() =
    lint goodNestedTryWithTest

  /// Groups nested across different constructs stay independent.
  [<TestMethod>]
  member _.``[LineBreak] Nested Construct Line Break Test``() =
    lint goodMatchInTryTest
    lintAssert badTryInCaseTest
    lintAssert badLambdaBodyCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Type Argument Line Break Test``() =
    lint goodTypeArgTest
    lint goodLongTypeArgTest
    lintAssert badTypeArgTest
