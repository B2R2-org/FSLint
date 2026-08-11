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
  /// A bit pattern built with '|||' reads as a row of fields, so its operands
  /// fill each line as far as it goes rather than standing one to a line. All
  /// that is asked is that a break be called for: an operand sent below while
  /// there was room beside its neighbour broke the row for nothing.
  [<TestMethod>]
  member _.``[App] Bitwise Chain Fill Test``() =
    (* The last operand does not fit above: 58 + 1 + 23 is past the budget. *)
    "let f p u w value =\n" +
    "  let imm =\n" +
    "    (1u <<< 11) ||| (p <<< 10) ||| (u <<< 9) ||| (w <<< 8)\n" +
    "    ||| unsignedImm 8 value\n" +
    "  imm\n"
    |> lint
    (* Here it does: the row was broken for nothing. *)
    "let f rt imm =\n" +
    "  wideWord (head rn)\n" +
    "           ((coreReg rt <<< 12) ||| (0xfu <<< 8)\n" +
    "             ||| offset (defaultArg imm 0L))\n"
    |> lintAssertMsg "Remove unnecessary line break"

  /// The budget decides, and it decides to the column: joined, the row below
  /// lands on the eightieth exactly and so had room, while one character more
  /// puts it past and the break was called for.
  [<TestMethod>]
  member _.``[App] Bitwise Chain Fill Boundary Test``() =
    let row tail =
      "let f p u w value =\n" +
      "  let imm =\n" +
      "    (1u <<< 11) ||| (p <<< 10) ||| (u <<< 9) ||| (w <<< 8)\n" +
      "    ||| unsignedImm 8 " + tail + "\n" +
      "  imm\n"
    (* 58 + 1 + 21 lands on the budget: there was room. *)
    row "abc" |> lintAssertMsg "Remove unnecessary line break"
    (* 58 + 1 + 22 is one past it: the break was called for. *)
    row "abcd" |> lint

  /// One operand to a line, where several would have fitted together.
  [<TestMethod>]
  member _.``[App] Bitwise Chain Fill Test(2)``() =
    "let f p u value =\n" +
    "  let imm =\n" +
    "    (1u <<< 11)\n" +
    "    ||| (p <<< 10)\n" +
    "    ||| unsignedImm 8 value\n" +
    "  imm\n"
    |> lintAssertMsg "Remove unnecessary line break"
