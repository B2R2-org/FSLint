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

  let goodBindingWithObjExprTest =
    """
let good =
  let a =
    { new ITest with
      member _.Foo() = ()
      member _.Bar() = () }
  0
"""

  let goodBindingWithObjExprTest2 =
    """
let good =
  let a =
    { new ITest with
      member _.Foo() = ()

      member _.Bar() = () }
  0
"""

  let badBindingWithObjExprTest =
    """
let good =
  let a =
    { new ITest with
      member _.Foo() = ()


      member _.Bar() = () }
  0
"""

  let goodBindingWithNestedTest =
    """
let good =
  let fn =
    let x = 1
    let y = 2
    x + y
  and gn =
    let x = 1
    let y = 2
    x + y
  0
"""

  let badBindingWithNestedTest =
    """
let bad =
  let fn =
    let x = 1
    let y = 2
    x + y

  and gn =
    let x = 1
    let y = 2
    x + y
  0
"""

  [<TestMethod>]
  member _.``[FunctionBody] Empty NewLine Test``() =
    lint goodEmptyNewLineTest
    lintAssert badEmptyNewLineTest

  [<TestMethod>]
  member _.``[FunctionBody] Recursive Binding NewLine Test``() =
    lint goodBindingWithAndKeywordTest
    lintAssert badBindingWithAndKeywordTest

  [<TestMethod>]
  member _.``[FunctionBody] Object Expression NewLine With Binding Test``() =
    lint goodBindingWithObjExprTest
    lint goodBindingWithObjExprTest2
    lintAssert badBindingWithObjExprTest

  [<TestMethod>]
  member _.``[FunctionBody] Nested Binding Test``() =
    lint goodBindingWithNestedTest
    lintAssert badBindingWithNestedTest