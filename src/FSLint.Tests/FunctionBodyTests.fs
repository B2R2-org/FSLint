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

  let goodBindingWithAndDocCommentTest =
    """
/// This is fn.
let fn () =
  let x = 1
  let y = 2
  x + y

/// This is good.
and good = ()
"""

  let goodBindingWithAndLineCommentTest =
    """
// This is fn.
let fn () =
  let x = 1
  let y = 2
  x + y

// This is good.
and good = ()
"""

  let goodBindingWithAndBlockCommentTest =
    """
(* This is fn. *)
let fn () =
  let x = 1
  let y = 2
  x + y

(* This is good. *)
and good = ()
"""

  let badBindingWithAndDocCommentTest =
    """
/// This is fn.
let fn () =
  let x = 1
  let y = 2
  x + y


/// This is bad.
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

  let goodNestedBindingWithDocCommentTest =
    """
let good =
  /// fn doc
  let fn =
    let x = 1
    let y = 2
    x + y
  /// gn doc
  and gn =
    let x = 1
    let y = 2
    x + y
  0
"""

  let goodNestedBindingWithLineCommentTest =
    """
let good =
  // fn comment
  let fn =
    let x = 1
    let y = 2
    x + y
  // gn comment
  and gn =
    let x = 1
    let y = 2
    x + y
  0
"""

  let goodNestedBindingWithBlockCommentTest =
    """
let good =
  (* fn comment *)
  let fn =
    let x = 1
    let y = 2
    x + y
  (* gn comment *)
  and gn =
    let x = 1
    let y = 2
    x + y
  0
"""

  let badNestedBindingWithCommentTest =
    """
let bad =
  /// fn doc
  let fn =
    let x = 1
    let y = 2
    x + y

  /// gn doc
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
  member _.``[FunctionBody] Recursive Binding With Comment Test``() =
    lint goodBindingWithAndDocCommentTest
    lint goodBindingWithAndLineCommentTest
    lint goodBindingWithAndBlockCommentTest
    lintAssert badBindingWithAndDocCommentTest

  [<TestMethod>]
  member _.``[FunctionBody] Object Expression NewLine With Binding Test``() =
    lint goodBindingWithObjExprTest
    lint goodBindingWithObjExprTest2
    lintAssert badBindingWithObjExprTest

  [<TestMethod>]
  member _.``[FunctionBody] Nested Binding Test``() =
    lint goodBindingWithNestedTest
    lintAssert badBindingWithNestedTest

  [<TestMethod>]
  member _.``[FunctionBody] Nested Binding With Comment Test``() =
    lint goodNestedBindingWithDocCommentTest
    lint goodNestedBindingWithLineCommentTest
    lint goodNestedBindingWithBlockCommentTest
    lintAssert badNestedBindingWithCommentTest