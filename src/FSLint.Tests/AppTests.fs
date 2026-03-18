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
