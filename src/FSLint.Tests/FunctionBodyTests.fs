namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type FunctionBodyTests() =

  let goodEmptyNewLineTest =
    """
let fn () =
  let x = 1
  let y = 2
  x + y
"""

  let badEmptyNewLineTest =
    """
let fn () =
  let x = 1
  let y = 2

  x + y
"""

  let goodBindingWithAndKeywordTest =
    """
let fn () =
  let x = 1
  let y = 2
  x + y

and good = ()
"""

  let badBindingWithAndKeywordTest =
    """
let fn () =
  let x = 1
  let y = 2
  x + y
and bad = ()
"""

  [<TestMethod>]
  member _.``[FunctionBody] Empty NewLine Test``() =
    lint goodEmptyNewLineTest
    lintAssert badEmptyNewLineTest

  [<TestMethod>]
  member _.``[FunctionBody] Recursive Binding NewLine Test``() =
    lint goodBindingWithAndKeywordTest
    lintAssert badBindingWithAndKeywordTest