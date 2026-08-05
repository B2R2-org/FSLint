namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// Sibling constructs must agree on their layout: either everything stays on
/// one line, or everything breaks onto a line of its own.
///
/// For a body hanging off a keyword, fitting on one line settles it first: when
/// every body of the group could sit beside its keyword inside the line budget,
/// every one of them has to, and only when at least one cannot, being too wide
/// or needing several lines of its own, does the group fall back to the weaker
/// demand that all of them break away.
///
/// A separator list is settled the same way round. While the whole list would
/// close up onto one line inside the budget it has to stay closed up, and only
/// once it would not does the older demand take over, that every gap between
/// neighbours agree.
[<TestClass>]
type LineBreakTests() =

  let goodBranchInlineTest =
    """
    if foo then printfn "good" else printfn "good2"
"""

  /// The 'then' body is too wide to travel back up beside its keyword, and one
  /// such body settles the layout of the whole chain: every branch breaks.
  let goodBranchBrokenTest =
    """
    if foo then
      printfn "a message that is much too long to sit beside its keyword here"
    else
      printfn "good2"
"""

  let goodBranchElifTest =
    """
    if foo then
      printfn "a message that is much too long to sit beside its keyword here"
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

  /// Short enough to close up onto one line, and so it has to.
  let badConditionShortTest =
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

  /// One case body too wide to travel back up beside its arrow, and the whole
  /// match settles on the broken layout.
  let goodCaseBrokenTest =
    """
    match value with
    | 1 ->
      printfn "a message that is much too long to sit beside its arrow there"
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
        printfn "a message that is much too long to sit beside its arrow here"
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

  /// A short parameter list spread over several lines would close up well
  /// inside the budget, so it has to.
  let badCtorParamShortTest =
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
type TestClass(lookup: Map<AnExtremelyLongKeyTypeNameHere,
                           AnExtremelyLongValueTypeName>,
               parameterNumberTwo: int,
               parameterNumberThree: int) =
  member _.Param2 = parameterNumberTwo
"""

  /// Only one separator here, so there is nothing for it to disagree with:
  /// 'param2' following 'lookup' on its closing line is simply un-broken.
  let goodSingleSeparatorTest =
    """
type TestClass(lookup: Map<AnExtremelyLongKeyTypeNameHere,
                           AnExtremelyLongValueTypeName>, param2: int) =
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
    let func (parameterNumberOne: int)
             (parameterNumberTwo: int)
             (parameterNumberThree: int) = parameterNumberOne
"""

  let badFuncParamTest =
    """
    let func (parameterOne: int) (parameterTwo: int) (parameterThree: int)
             (parameterFour: int) = parameterOne
"""

  /// A type argument list is a group of its own, held to the same rule.
  let goodTypeArgTest =
    """
type TestClass(lookup: Map<AnExtremelyLongKeyTypeNameHere,
                           AnExtremelyLongValueTypeName>) =
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
      printfn "a message that is much too long to sit beside its keyword here"
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

  /// The condition and the branch bodies are separate groups, but only one way
  /// round. A condition split over several lines is one that would not close up
  /// inside the budget, so the 'if' cannot be written on one line at all and
  /// every branch body has to break with it. Leaving them beside their keywords
  /// is reported.
  let badBrokenConditionInlineBranchTest =
    """
    if isSomethingRatherLongHere &&
       isAnotherRatherLongCondition &&
       isYetAnotherLongCondition then printfn "good"
    else printfn "good2"
"""

  /// A condition that fits on one line does not oblige its branches either way,
  /// but the chain as a whole still has to close up while it can: with the
  /// 'else' left below, this one is reported for the break, not the mixture.
  let badInlineConditionInlineBranchTest =
    """
    if foo && bar && baz then printfn "good"
    else printfn "good2"
"""

  let goodInlineConditionBrokenBranchTest =
    """
    if foo && bar && baz then
      printfn "a message that is much too long to sit beside its keyword here"
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

  /// A single case has nothing to disagree with, yet it still has to sit beside
  /// its arrow for as long as it fits.
  let goodSingleCaseTest =
    """
    match value with
    | _ -> printfn "good"
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
        | 1 -> printfn "good"
        | _ -> printfn "good2"
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
    let func (parameterNumberOne: int,
              parameterNumberTwo: int,
              parameterNumberThree: int) = parameterNumberOne
"""

  let badTupledParamTest =
    """
    let func (aaa: int, bbb: int,
              ccc: int) = aaa
"""

  let goodMemberParamTest =
    """
type TestClass() =
  member _.Method(parameterNumberOne: int,
                  parameterNumberTwo: int,
                  parameterNumberThree: int) = parameterNumberOne
"""

  let badMemberParamTest =
    """
type TestClass() =
  member _.Method(aaa: int, bbb: int,
                  ccc: int) = aaa
"""

  /// 'try' and its handler form a group of their own. The handler is measured
  /// from its '->', the same anchor a `match` case uses, so where the 'with'
  /// keyword sits does not enter into it. But the whole of a bar-less 'try'
  /// that would close up onto one line has to be on one line first, so both
  /// shapes below are reported for the break rather than for the pairing.
  let badTryWithInlineTest =
    """
    let func () =
      try foo ()
      with ex -> bar ()
"""

  let goodTryWithOneLineTest =
    """
    let func () = try foo () with ex -> bar ()
"""

  let badTryWithKeywordOwnLineTest =
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
        printfn "a message far too long to be pulled back up beside its arrow"
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

  /// A barred handler hangs off the 'with' keyword, so it is the leading '|'
  /// that has to agree with the broken 'try' body. The case bodies answer to
  /// each other alone and may stay beside their arrows.
  let goodTryWithBarredCaseTest =
    """
    let func () =
      try
        foo ()
      with
      | :? System.IO.IOException -> bar ()
      | :? System.TimeoutException -> baz ()
"""

  /// The cases agree with each other; only the try/with pairing is off, the
  /// 'try' body sitting inline while the bars break onto their own lines.
  let badTryWithBarredCaseTest =
    """
    let func () =
      try foo ()
      with
      | :? System.IO.IOException -> bar ()
      | :? System.TimeoutException -> baz ()
"""

  /// The handler body broken out below its arrow, matching the broken 'try'
  /// body. This is the canonical multi-line shape, and what keeps it out of the
  /// one-line layout is the width of the handler body.
  let goodTryWithPatternOnKeywordLineTest =
    """
    let func () =
      try
        foo ()
      with ex ->
        printfn "a message far too long to be pulled back up beside its arrow"
"""

  let goodTryWithTypedHandlerTest =
    """
    let func () =
      try
        foo ()
      with :? System.IO.IOException as ex ->
        printfn "a message far too long to be pulled back up beside its arrow"
"""

  /// 'try' pairs with 'finally' exactly as it does with 'with', and closes up
  /// onto one line on the same terms.
  let goodTryFinallyOneLineTest =
    """
    let func () = try foo () finally bar ()
"""

  let badTryFinallyInlineTest =
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
        printfn "a message far too long to be pulled back up beside a keyword"
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
type TestClass<'aLongTypeParameterName,
               'bLongTypeParameterName,
               'cLongTypeParameterName>() =
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
type TestClass private(parameterNumberOne: int,
                       parameterNumberTwo: int,
                       parameterNumberThree: int) =
  member _.Aaa = parameterNumberOne
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
    let func (x: Map<int, List<AnExtremelyLongTypeNameHere,
                               AnotherExtremelyLongTypeName>>) = x
"""

  /// Only '||' in the chain, and only the second separator breaks.
  let badOrOnlyConditionTest =
    """
    if isSomethingRatherLongHere || isAnotherRatherLongCondition ||
       isYetAnotherLongCondition then
      printfn "bad"
    else
      printfn "bad2"
"""

  /// Each 'elif' link carries a condition group of its own, and a break in any
  /// one of them keeps every branch of the chain broken.
  let badElifConditionTest =
    """
    if foo then
      printfn "bad"
    elif isSomethingRatherLongHere && isAnotherRatherLongCondition &&
         isYetAnotherLongCondition then
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
    if someFunction (aaaaaaaaaa,
                     bbbbbbbbbb) && anotherRatherLongCondition &&
       aThirdRatherLongCondition then
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
      printfn "a message that is much too long to sit beside its keyword here"
    else
      printfn "good2"
"""

  /// A single type argument has no gap for a break to land between, but the
  /// angle brackets it sits inside are ends of their own, and this one would
  /// close up onto the line above with room to spare.
  let badSingleTypeArgTest =
    """
    let func (x: Option<
                   int>) = x
"""

  /// The same shape, with an argument that genuinely will not fit up there.
  let goodSingleWideTypeArgTest =
    """
    let func (x: Option<
                   AnExtremelyLongTypeNameThatWillNotFitOnTheLineAboveIt>) = x
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
type TestClass<'aLongTypeParameterName,
               'bLongTypeParamName when 'bLongTypeParamName: comparison>() =
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
        printfn "a message far too long to be pulled back up beside its arrow"
"""

  let badGuardedHandlerTest =
    """
    let func () =
      try
        foo ()
      with ex when ex <> null -> bar ()
"""

  /// The inner 'try' fits on one line and so must sit on one; that makes the
  /// outer 'try' body several lines long, which settles the outer group on the
  /// broken layout.
  let goodNestedTryWithTest =
    """
    let func () =
      try
        try inner () with e1 -> ()
      with e2 ->
        cleanup ()
        report ()
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
type TestClass(lookup: Map<AnExtremelyLongKeyTypeNameHere,
                           AnExtremelyLongValueTypeName,
                           AnExtremelyLongExtraTypeName>) =
  member _.Lookup = lookup
"""

  (* --------------------------------------------------------------------- *)
  (* Fitting on one line comes before agreeing on a layout.                 *)
  (* --------------------------------------------------------------------- *)

  /// Both branches would fit beside their keywords, so breaking them out is
  /// reported even though the two agree with each other perfectly well.
  let badShortBranchBrokenTest =
    """
    if foo then
      printfn "bad"
    else
      printfn "bad2"
"""

  /// The same holds all the way along an 'elif' chain.
  let badShortElifBrokenTest =
    """
    if foo then
      printfn "bad"
    elif bar then
      printfn "bad2"
    else
      printfn "bad3"
"""

  /// One body too wide to come up settles the chain on the broken layout, and
  /// the branch left inline beside its keyword is what gets reported.
  let badWideBranchMixedTest =
    """
    if foo then
      printfn "a message that is much too long to sit beside its keyword here"
    else printfn "bad2"
"""

  /// A body of two statements needs lines of its own, since we write no ';', so
  /// it can never come up, and the whole chain stays broken.
  let goodSequentialBranchTest =
    """
    if foo then
      printfn "good"
      printfn "good2"
    else
      printfn "good3"
"""

  let badSequentialBranchTest =
    """
    if foo then
      printfn "bad"
      printfn "bad2"
    else printfn "bad3"
"""

  /// A body that opens with a binding is several lines long for the same
  /// reason, and settles the chain the same way.
  let goodLetBranchTest =
    """
    if foo then
      let temp = compute foo
      temp + 1
    else
      0
"""

  /// Every case would fit beside its arrow, so none of them may break away.
  let badShortCaseBrokenTest =
    """
    match value with
    | 1 ->
      printfn "bad"
    | _ ->
      printfn "bad2"
"""

  /// One case too wide to come up, and the short one left inline is reported.
  let badWideCaseMixedTest =
    """
    match value with
    | 1 ->
      printfn "a message that is much too long to sit beside its arrow there"
    | _ -> printfn "bad2"
"""

  /// A lone case has nothing to agree with, yet it still has to come up while
  /// it fits.
  let badSingleCaseBrokenTest =
    """
    match value with
    | _ ->
      printfn "bad"
"""

  /// Too wide to come up, and with no sibling to disagree with, it is left be.
  let goodSingleWideCaseTest =
    """
    match value with
    | _ ->
      printfn "a message that is much too long to sit beside its arrow there"
"""

  /// A comment in the gap would be swallowed by the join, so the case stays
  /// where it is and takes the rest of the match with it.
  let goodCommentedCaseTest =
    """
    match value with
    | 1 ->
      (* The reason this case is worth a word of its own. *)
      printfn "good"
    | _ ->
      printfn "good2"
"""

  let badCommentedCaseTest =
    """
    match value with
    | 1 ->
      (* The reason this case is worth a word of its own. *)
      printfn "bad"
    | _ -> printfn "bad2"
"""

  /// A case body of two statements keeps the whole match broken.
  let goodSequentialCaseTest =
    """
    match value with
    | 1 ->
      printfn "good"
      printfn "good2"
    | _ ->
      printfn "good3"
"""

  /// The budget is measured to the column: a body landing on the eightieth
  /// belongs beside its arrow.
  let goodBoundaryCaseTest =
    """
    match value with
    | 1 -> printfn "this message is just wide enough to fit on the last column."
    | _ -> ()
"""

  let badBoundaryCaseTest =
    """
    match value with
    | 1 ->
      printfn "this message is just wide enough to fit on the last column."
    | _ -> ()
"""

  /// One column further and the body has to stay where it is, taking its
  /// sibling with it.
  let goodOverBoundaryCaseTest =
    """
    match value with
    | 1 ->
      printfn "this message is a single column too wide to fit on the line."
    | _ ->
      ()
"""

  /// 'try' and its handler are held to the budget just as branches are.
  let badShortTryWithBrokenTest =
    """
    let func () =
      try
        foo ()
      with ex ->
        bar ()
"""

  let badShortTryFinallyBrokenTest =
    """
    let func () =
      try
        foo ()
      finally
        bar ()
"""

  /// A short type argument list closes up just as a parameter list does.
  let badShortTypeArgBrokenTest =
    """
type TestClass(lookup: Map<int,
                           string>) =
  member _.Lookup = lookup
"""

  let badShortTypeParamBrokenTest =
    """
type TestClass<'a,
               'b,
               'c>() =
  member _.Value = 1
"""

  let badShortCurriedParamBrokenTest =
    """
    let func (a: int)
             (b: int)
             (c: int) = a + b + c
"""

  /// A comment between two elements would be swallowed by closing the list up,
  /// so the list stays as it is.
  let goodCommentedListTest =
    """
type TestClass(param1: int,
               (* The second one, which is worth a word. *)
               param2: int) =
  member _.Param1 = param1
"""

  /// ... and with the list settled on the broken layout, the separator left
  /// un-broken is what gets reported.
  let badCommentedListTest =
    """
type TestClass(param1: int,
               (* The second one, which is worth a word. *)
               param2: int, param3: int) =
  member _.Param1 = param1
"""

  /// The budget is measured to the column here too: a list closing up onto the
  /// eightieth belongs on one line.
  let goodListBoundaryTest =
    """
type TestClass(parameterNumberOneIsHere1: int, parameterNumberTwoIsHere2: int) =
  member _.Value = 1
"""

  let badListBoundaryTest =
    """
type TestClass(parameterNumberOneIsHere1: int,
               parameterNumberTwoIsHere2: int) =
  member _.Value = 1
"""

  /// One column further and the list is free to stay broken.
  let goodListOverBoundaryTest =
    """
type TestClass(parameterNumberOneIsHere12: int,
               parameterNumberTwoIsHere2: int) =
  member _.Value = 1
"""

  /// The body that will not fit is the one left inline this time. One such
  /// settles the chain all the same, and the report lands on it. Its line is
  /// over the budget too, so this one is read off the whole error list.
  let badWideInlineBranchTest =
    "\n    if foo then printfn " +
    "\"this message is deliberately long enough to run past eighty\"\n" +
    "    else\n" +
    "      printfn \"bad2\"\n"

  /// A lone case over the budget has no sibling to break away with, so its
  /// width is left to the line rule and the group says nothing of its own.
  let goodWideInlineSingleCaseTest =
    "\n    match value with\n" +
    "    | _ -> printfn " +
    "\"this message is deliberately long enough to run past eighty!\"\n"

  /// A '|' left beside its 'with' is reported, since a barred handler can only
  /// ever hang below it.
  let badBarBesideWithTest =
    """
    let func () =
      try
        foo ()
      with | :? System.IO.IOException -> bar ()
           | _ -> baz ()
"""

  /// A lone bar-less handler is no case list of its own: its '->' is the anchor
  /// the try/with pairing is measured from. With the 'try' body too long to
  /// come up, the handler breaks with it rather than being pulled up alone.
  let goodLoneHandlerFollowsTryTest =
    """
    let func () =
      try
        foo ()
        bar ()
      with ex ->
        baz ()
"""

  /// The stretch runs past its last element to the bracket that closes it, so a
  /// break landing there is a break in the list. With nothing further down to
  /// point at, the last element takes the report.
  let badTrailingBracketBreakTest =
    """
type TestClass(aaa: int, bbb: int
              ) =
  member _.Aaa = aaa
"""

  /// Where a bracket meets what it fences in the closed-up form has no space
  /// between them, and this argument lands on the eightieth column exactly. A
  /// space wrongly counted at that junction would push it to the eighty-first
  /// and let it off, so the pair below pins the junction as well as the budget.
  let badBracketJunctionBoundaryTest =
    """
    let func (x: Option<
                   AnExtremelyLongTypeNameLandingRightOnTheLastColumn>) = x
"""

  let goodBracketJunctionOverBoundaryTest =
    """
    let func (x: Option<
                   AnExtremelyLongTypeNameLandingJustPastTheLastColumn>) = x
"""

  /// The chain closes up onto one line while it fits, so the 'else' below is
  /// reported even though its body sits neatly beside it.
  let badElseOnOwnLineTest =
    """
    if ins.Flag then pushToStack bld (AST.undef rt "NULL")
    else ()
"""

  /// One column too wide to close up, and the two-line shape is what is left.
  let goodElseOnOwnLineTest =
    """
    if ins.Flag then pushToStack bld (AST.undef rt "NULL_POINTER_VALUE_HERE")
    else ()
"""

  /// A branch body needing lines of its own puts the one-line shape out of
  /// reach however short the chain reads.
  let goodSequentialKeepsBranchesTest =
    """
    if foo then
      printfn "good"
      printfn "good2"
    else
      ()
"""

  /// A bar-less 'try' closes up on the same terms as an 'if'.
  let badTryClosesUpTest =
    """
    let func () =
      try riskyOp ()
      with ex -> report ex
"""

  let goodTryTooWideToCloseTest =
    """
    let func () =
      try riskyOp ()
      with ex -> report ex "a rather long explanation of what went wrong"
"""

  /// A barred handler can never close up, whatever room the line has left.
  let goodBarredHandlerNeverClosesTest =
    """
    let func () =
      try
        riskyOp ()
      with
      | :? System.IO.IOException -> report ()
      | ex -> report ex
"""

  /// A compiler directive between a keyword and its body cannot move, so the
  /// body cannot come up past it however much room the line has left, and the
  /// whole group settles on the broken layout.
  let goodDirectiveInGapTest =
    """
    let func x =
      match x with
      | 1 ->
#if DEBUG
        printfn "good"
#else
        printfn "good2"
#endif
      | _ ->
        printfn "good3"
"""

  /// ... and with the group settled that way, the case left inline is the one
  /// reported.
  let badDirectiveInGapTest =
    """
    let func x =
      match x with
      | 1 ->
#if DEBUG
        printfn "bad"
#else
        printfn "bad2"
#endif
      | _ -> printfn "bad3"
"""

  /// A closing bracket that opens a line joins tight when the list closes up,
  /// so 'Map<int, string>' is measured at the width it would really have.
  let badClosingBracketOwnLineTest =
    """
    let func (x: Map<int,
                     string
                     >) = x
"""

  /// The outer condition fits on one line but an 'elif' further along does not,
  /// and that settles the layout of every branch in the chain.
  let badElifBrokenConditionBranchTest =
    """
    if foo then printfn "bad"
    elif isSomethingRatherLongHere &&
         isAnotherRatherLongCondition &&
         isYetAnotherLongCondition then
      printfn "bad2"
    else
      printfn "bad3"
"""

  /// However much room the line has left, a '|' never comes up to join the
  /// 'with' it hangs from, so a barred handler keeps the group broken.
  let goodBarredHandlerRoomTest =
    """
    let func () =
      try
        foo ()
      with
      | ex -> bar ()
      | ex2 -> baz ()
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
    lintAssertMsg "Use consistent line breaks" badConditionTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test(2)``() =
    lint goodConditionMixedTest
    lint goodConditionParenTest
    lintAssertMsg "Use consistent line breaks" badConditionPrecedenceTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test(3)``() =
    lintAssertMsg "Use consistent line breaks" badConditionFirstBreakTest

  /// A broken condition settles the branches; an unbroken one leaves them to
  /// their own widths.
  [<TestMethod>]
  member _.``[LineBreak] Group Independence Test``() =
    lint goodBranchInlineTest
    lint goodInlineConditionBrokenBranchTest
    lintAssertMsg "Use consistent line breaks"
      badBrokenConditionInlineBranchTest
    lintAssertMsg "Remove unnecessary line break"
      badInlineConditionInlineBranchTest

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
    lint goodTryWithOneLineTest
    lintAssertMsg "Remove unnecessary line break" badTryWithInlineTest
    lintAssertMsg "Remove unnecessary line break" badTryWithKeywordOwnLineTest
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
  member _.``[LineBreak] Try With Barred Case Line Break Test``() =
    lint goodTryWithBarredCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Try With Barred Case Line Break Test(2)``() =
    lintAssert badTryWithBarredCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Try Finally Line Break Test``() =
    lint goodTryFinallyOneLineTest
    lint goodTryFinallyBrokenTest
    lintAssertMsg "Remove unnecessary line break" badTryFinallyInlineTest
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
    lintAssertMsg "Use consistent line breaks" badOrOnlyConditionTest
    lintAssertMsg "Use consistent line breaks" badElifConditionTest

  [<TestMethod>]
  member _.``[LineBreak] Condition Line Break Test(5)``() =
    lintAssertMsg "Use consistent line breaks" badMiddleConditionTest
    lintAssertMsg "Use consistent line breaks" badMultiLineOperandTest

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

  /// A group with a single item has nothing to disagree with, and where it has
  /// no brackets either there is no layout left to judge at all.
  [<TestMethod>]
  member _.``[LineBreak] Single Item Group Test``() =
    lint goodSingleOperandConditionTest
    lint goodSingleCtorParamTest

  /// Brackets give a lone element somewhere for a break to land, so it is held
  /// to the budget like any other list.
  [<TestMethod>]
  member _.``[LineBreak] Single Bracketed Item Test``() =
    lint goodSingleWideTypeArgTest
    lintAssertMsg "Remove unnecessary line break" badSingleTypeArgTest

  /// The over-wide body being the inline one settles the chain just as surely.
  /// Its line breaks the budget as well, so the group's own report is picked
  /// out of the full list rather than caught as the first thrown error.
  [<TestMethod>]
  member _.``[LineBreak] Wide Inline Body Test``() =
    lintErrors badWideInlineBranchTest
    |> List.filter (fun e -> e.Message = "Use consistent line breaks")
    |> fun errors -> Assert.AreEqual<int>(1, errors.Length)

  /// A lone over-wide body has nothing to break away with, so the group adds
  /// nothing to what the line rule already says.
  [<TestMethod>]
  member _.``[LineBreak] Wide Inline Single Case Test``() =
    let errors = lintErrors goodWideInlineSingleCaseTest
    errors
    |> List.filter (fun e -> e.Message.Contains "exceeds")
    |> fun tooWide -> Assert.AreEqual<int>(1, tooWide.Length)
    errors
    |> List.filter (fun e -> e.Message = "Use consistent line breaks")
    |> fun breaks -> Assert.AreEqual<int>(0, breaks.Length)

  [<TestMethod>]
  member _.``[LineBreak] Bar Beside With Test``() =
    lintAssertMsg "Use consistent line breaks" badBarBesideWithTest

  /// The pairing owns a lone bar-less handler, so the case rule does not pull
  /// its body up behind the pairing's back.
  [<TestMethod>]
  member _.``[LineBreak] Lone Handler Follows Try Test``() =
    lint goodLoneHandlerFollowsTryTest

  /// A break landing between the last element and its closing bracket is still
  /// a break in the list.
  [<TestMethod>]
  member _.``[LineBreak] Trailing Bracket Break Test``() =
    lintErrors badTrailingBracketBreakTest
    |> List.filter (fun e -> e.Message = "Remove unnecessary line break")
    |> fun errors -> Assert.AreEqual<int>(1, errors.Length)

  /// A construct that would close up onto one line has to be on one line, so
  /// an 'else' left below is reported however neatly its body sits beside it.
  [<TestMethod>]
  member _.``[LineBreak] Chain Closes Up On One Line Test``() =
    lint goodElseOnOwnLineTest
    lint goodSequentialKeepsBranchesTest
    lintAssertMsg "Remove unnecessary line break" badElseOnOwnLineTest

  /// A 'try' is held to the same demand, and a barred handler is exempt from it
  /// because its '|' can never join the 'with' above.
  [<TestMethod>]
  member _.``[LineBreak] Try Closes Up On One Line Test``() =
    lint goodTryTooWideToCloseTest
    lint goodBarredHandlerNeverClosesTest
    lintAssertMsg "Remove unnecessary line break" badTryClosesUpTest

  /// A body reachable only through a compiler directive stays where it is.
  [<TestMethod>]
  member _.``[LineBreak] Directive In Gap Test``() =
    lint goodDirectiveInGapTest
    lintAssertMsg "Use consistent line breaks" badDirectiveInGapTest

  [<TestMethod>]
  member _.``[LineBreak] Closing Bracket On Own Line Test``() =
    lintAssertMsg "Remove unnecessary line break" badClosingBracketOwnLineTest

  /// The bracket junction is measured tight, to the column.
  [<TestMethod>]
  member _.``[LineBreak] Bracket Junction Boundary Test``() =
    lint goodBracketJunctionOverBoundaryTest
    lintAssertMsg "Remove unnecessary line break"
      badBracketJunctionBoundaryTest

  [<TestMethod>]
  member _.``[LineBreak] Elif Broken Condition Branch Test``() =
    lintAssertMsg "Use consistent line breaks" badElifBrokenConditionBranchTest

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

  /// A group that fits on one line has to be on one line, however neatly its
  /// members agree with each other on breaking away.
  [<TestMethod>]
  member _.``[LineBreak] Branch Fits On One Line Test``() =
    lintAssertMsg "Remove unnecessary line break" badShortBranchBrokenTest
    lintAssertMsg "Remove unnecessary line break" badShortElifBrokenTest

  /// Once one body cannot come up the group falls back on the weaker demand,
  /// and it is the branch left inline that is reported.
  [<TestMethod>]
  member _.``[LineBreak] Branch Too Wide To Join Test``() =
    lintAssertMsg "Use consistent line breaks" badWideBranchMixedTest

  /// A body needing lines of its own can never come up, whatever room is left.
  [<TestMethod>]
  member _.``[LineBreak] Multi Line Branch Body Test``() =
    lint goodSequentialBranchTest
    lint goodLetBranchTest
    lintAssertMsg "Use consistent line breaks" badSequentialBranchTest

  [<TestMethod>]
  member _.``[LineBreak] Case Fits On One Line Test``() =
    lintAssertMsg "Remove unnecessary line break" badShortCaseBrokenTest
    lintAssertMsg "Remove unnecessary line break" badSingleCaseBrokenTest

  [<TestMethod>]
  member _.``[LineBreak] Case Too Wide To Join Test``() =
    lint goodSingleWideCaseTest
    lintAssertMsg "Use consistent line breaks" badWideCaseMixedTest

  [<TestMethod>]
  member _.``[LineBreak] Multi Line Case Body Test``() =
    lint goodSequentialCaseTest

  /// Joining would swallow a comment sitting in the gap, so it cannot happen.
  [<TestMethod>]
  member _.``[LineBreak] Commented Case Body Test``() =
    lint goodCommentedCaseTest
    lintAssertMsg "Use consistent line breaks" badCommentedCaseTest

  /// The budget is measured to the column, and both sides of it are checked.
  [<TestMethod>]
  member _.``[LineBreak] Line Budget Boundary Test``() =
    lint goodBoundaryCaseTest
    lint goodOverBoundaryCaseTest
    lintAssertMsg "Remove unnecessary line break" badBoundaryCaseTest

  [<TestMethod>]
  member _.``[LineBreak] Try Fits On One Line Test``() =
    lintAssertMsg "Remove unnecessary line break" badShortTryWithBrokenTest
    lintAssertMsg "Remove unnecessary line break" badShortTryFinallyBrokenTest

  [<TestMethod>]
  member _.``[LineBreak] Barred Handler Never Joins Test``() =
    lint goodBarredHandlerRoomTest

  /// A list that would close up onto one line has to be on one line, whether it
  /// holds parameters, type arguments or type parameters.
  [<TestMethod>]
  member _.``[LineBreak] List Closes Up On One Line Test``() =
    lintAssertMsg "Remove unnecessary line break" badCtorParamShortTest
    lintAssertMsg "Remove unnecessary line break" badShortTypeArgBrokenTest
    lintAssertMsg "Remove unnecessary line break" badShortTypeParamBrokenTest

  [<TestMethod>]
  member _.``[LineBreak] List Closes Up On One Line Test(2)``() =
    lintAssertMsg "Remove unnecessary line break" badShortCurriedParamBrokenTest
    lintAssertMsg "Remove unnecessary line break" badConditionShortTest

  /// Closing up would swallow a comment sitting between two elements.
  [<TestMethod>]
  member _.``[LineBreak] Commented List Test``() =
    lint goodCommentedListTest
    lintAssertMsg "Use consistent line breaks" badCommentedListTest

  [<TestMethod>]
  member _.``[LineBreak] List Budget Boundary Test``() =
    lint goodListBoundaryTest
    lint goodListOverBoundaryTest
    lintAssertMsg "Remove unnecessary line break" badListBoundaryTest
