namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type AppTests() =

  let goodListSpaceInfixTest =
    """
[ this.Address + uint64 this.Length + this.Name ]
"""

  let badListSpaceInfixTest =
    """
[ this.Address +  uint64 this.Length + this.Name ]
"""

  let goodListAppIndexerTest =
    """
[ lifter.File.RawBytes[ptr.Offset] ]
"""

  let badListAppIndexerTest =
    """
[ lifter.File.RawBytes[ ptr.Offset ] ]
"""

  let goodListAppIndexerInRangeTest =
    """
good[1..]
"""

  let badListAppIndexerInRangeTest =
    """
bad[ 1.. ]
"""

  let goodListSpaceFunAppTest = """[ fn 1 2 3 x ]"""

  let badListSpaceFunAppTest = """[ fn  1 2 3 x ]"""

  let goodMethodParenCallTest =
    """
let content = File.ReadAllText(filePath)
"""

  let goodGenericMethodParenCallTest =
    """
let value = strictVal.Value<bool>()
"""

  let goodNamedArgumentSpacingTest =
    """
func (param = value)
"""

  let badNamedArgumentSpacingTest =
    """
func (param=value)
"""

  let goodPipelineCommentMultilineTest =
    """
|> getBankedReg (pickBit bin 20) (* R *)
|> OprReg
"""

  let goodPipelineCommentSameLineTest =
    """
|> getBankedReg (pickBit bin 20) (* R *) |> OprReg
"""

  let badGenericArguSpacingParsedAsInfixTest =
    """
let bad = genericFn <int> 10
"""

  let badGenericMultiArguSpacingParsedAsInfixTest =
    """
let bad = genericFn2 <int, string> 1 "x"
"""

  let goodInfixOperatorInCondTest =
    """
let f x = if x.Y > 42 then x.X else -x.X + 1
"""

  [<TestMethod>]
  member _.``[App] List Space Before and After Infix Test``() =
    lint goodListSpaceInfixTest
    lintAssert badListSpaceInfixTest

  [<TestMethod>]
  member _.``[App] List App Indexer Bracket Spacing Test``() =
    lint goodListAppIndexerTest
    lintAssert badListAppIndexerTest

  [<TestMethod>]
  member _.``[App] List App Indexer Index Range Test``() =
    lint goodListAppIndexerInRangeTest
    lintAssert badListAppIndexerInRangeTest

  [<TestMethod>]
  member _.``[App] List Space Fununction Application Test``() =
    lint goodListSpaceFunAppTest
    lintAssert badListSpaceFunAppTest

  [<TestMethod>]
  member _.``[App] Method Paren Call Should Not Be Function App``() =
    lint goodMethodParenCallTest

  [<TestMethod>]
  member _.``[App] Generic Method Paren Call Should Not Be Function App``() =
    lint goodGenericMethodParenCallTest

  [<TestMethod>]
  member _.``[App] Named Argument Assignment Spacing Test``() =
    lint goodNamedArgumentSpacingTest
    lintAssert badNamedArgumentSpacingTest

  [<TestMethod>]
  member _.``[App] Pipeline With Trailing Comment Across Lines Test``() =
    lint goodPipelineCommentMultilineTest

  [<TestMethod>]
  member _.``[App] Pipeline With Inline Block Comment Test``() =
    lint goodPipelineCommentSameLineTest

  [<TestMethod>]
  member _.``[App] Generic Argument Spacing Parsed As Infix Test``() =
    lintAssert badGenericArguSpacingParsedAsInfixTest

  [<TestMethod>]
  member _.``[App] Generic Multi Argument Spacing Parsed As Infix Test``() =
    lintAssert badGenericMultiArguSpacingParsedAsInfixTest

  [<TestMethod>]
  member _.``[App] Infix Spacing In Condition Test``() =
    lint goodInfixOperatorInCondTest

  [<TestMethod>]
  member _.``[UnaryOp] Negation without space - good``() =
    "let negate x = -x\n" |> lint

  [<TestMethod>]
  member _.``[UnaryOp] Positive without space - good``() =
    "let positive x = +x\n" |> lint

  [<TestMethod>]
  member _.``[UnaryOp] Negation in expression - good``() =
    "let compute x = -x + 10\n" |> lint

  [<TestMethod>]
  member _.``[UnaryOp] Binary minus is allowed with spaces``() =
    "let subtract x y = x - y\n" |> lint

  [<TestMethod>]
  member _.``[UnaryOp] Error - space between negation and operand``() =
    "let negate x = - x\n" |> lintAssert

  [<TestMethod>]
  member _.``[UnaryOp] Error - space between positive and operand``() =
    "let positive x = + x\n" |> lintAssert

  [<TestMethod>]
  member _.``[UnaryOp] Error - space in complex expression``() =
    "let compute x = - x + 10\n" |> lintAssert

  [<TestMethod>]
  member _.``[UnaryOp] DEBUG - with space``() =
    try "let negate x = - x\n" |> lint with _ -> ()

  [<TestMethod>]
  member _.``[UnaryOp] DEBUG - without space``() =
    try "let negate x = -x\n" |> lint with _ -> ()

  /// A chain of bitwise operators is a separator list like the operands of a
  /// condition: while the whole of it would close up onto one line it stays
  /// closed up, and once it would not, every gap between its operands has to
  /// agree. A chain nests to the left, so its prefixes must not be judged
  /// again on their own: a short prefix of a long chain looks as though it
  /// could close up when the chain holding it cannot.
  [<TestMethod>]
  member _.``[App] Bitwise Chain Placement Test``() =
    "let numberFormat =\n" +
    "  NumberLiteralOptions.AllowBinary\n" +
    "  ||| NumberLiteralOptions.AllowOctal\n" +
    "  ||| NumberLiteralOptions.AllowHexadecimal\n" +
    "  ||| NumberLiteralOptions.AllowMinusSign\n" +
    "  ||| NumberLiteralOptions.AllowPlusSign\n"
    |> lint

  /// Too wide for one line, and packed rather than spread. Where a chain that
  /// will not fit is broken is the author's to choose, so nothing is asked.
  [<TestMethod>]
  member _.``[App] Bitwise Chain Placement Test(2)``() =
    "let f p u w value =\n" +
    "  let imm =\n" +
    "    (1u <<< 11) ||| (p <<< 10) ||| (u <<< 9) ||| (w <<< 8)\n" +
    "    ||| unsignedImm 8 value\n" +
    "  imm\n"
    |> lint

  /// A chain short enough to close up has to be closed up, however evenly it
  /// was spread.
  [<TestMethod>]
  member _.``[App] Bitwise Chain Placement Test(3)``() =
    "let f p u =\n" +
    "  let imm =\n" +
    "    (1u <<< 11)\n" +
    "    ||| (p <<< 10)\n" +
    "    ||| (u <<< 9)\n" +
    "  imm\n"
    |> lintAssertMsg "Remove unnecessary line break"
