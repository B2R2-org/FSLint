namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type NegationSimplificationTests() =

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a = b)``() =
    "let f a b = not (a = b)\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a <> b)``() =
    "let f a b = not (a <> b)\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a > b)``() =
    "let f a b = not (a > b)\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a >= b)``() =
    "let f a b = not (a >= b)\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a < b)``() =
    "let f a b = not (a < b)\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - not (a <= b)``() =
    "let f a b = not (a <= b)\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - in if expression``() =
    "let f x =\n" +
    "  if not (x > 0) then\n" +
    "    printfn \"negative\"\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - nested expression``() =
    "let f a b c = a + not (b = c)\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Good - using opposite operator``() =
    "let f a b = a <> b\n" |> lint

  [<TestMethod>]
  member _.``[NegationSimplification] Good - not with non-comparison``() =
    "let f a = not (isValid a)\n" |> lint

  [<TestMethod>]
  member _.``[NegationSimplification] Good - not with boolean literal``() =
    "let f = not true\n" |> lint

  [<TestMethod>]
  member _.``[NegationSimplification] Good - not with complex expression``() =
    "let f a b = not (a > 0 && b < 0)\n" |> lint

  [<TestMethod>]
  member _.``[NegationSimplification] Error - pipeline not``() =
    "let f a b = a = b |> not\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - pipeline with paren not``() =
    "let f a b = (a = b) |> not\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Error - pipeline not ``() =
    "let f a b = a > b |> not\n" |> lintAssert

  [<TestMethod>]
  member _.``[NegationSimplification] Good - pipeline non-comparison``() =
    "let f a = isValid a |> not\n" |> lint