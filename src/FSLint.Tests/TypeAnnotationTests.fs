namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type TypeAnnotationTest() =
  let goodEmptyParenTest =
    """
let fn () = ()
"""

  let badEmptyParenTest =
    """
let fn ( ) = ()
"""

  let goodTypeAnnotationIntArrayTest =
    """
let fn (param: int[]) = 10
"""

  let badTypeAnnotationIntArrayTest =
    """
let fn (param: int []) = 10
"""

  let goodTypeAnnotationIntTest =
    """
let fn (p: int) = 10
"""

  let badTypeAnnotationIntTest =
    """
let fn (p:int) = 10
"""

  let goodTypeAnnotationStringTest =
    """
let fn (str: string) = "10"
"""

  let badTypeAnnotationStringTest =
    """
let fn (str:string) = "10"
"""

  let goodTypeAnnotationStringSpaceTest =
    """
let fn (str: string) = "10"
"""

  let badTypeAnnotationStringSpaceTest =
    """
let fn (str :string) = "10"
"""

  let goodTypeAnnotationArrayTest =
    """
let inline toString (stmts: LowUIR.Stmt[]) = ()
"""

  let badTypeAnnotationArrayTest =
    """
let inline toString (stmts:  LowUIR.Stmt[]) = ()
"""

  let goodTypeAnnotationColonSpacingTest =
    """
type X = X of a: int * b: int
"""

  let badTypeAnnotationColonSpacingTest =
    """
type X = X of a : int * b: int
"""

  let goodTypeAnnotationStarSpacingTest =
    """
type X = X of a: int * b: int
"""

  let badTypeAnnotationStarSpacingTest =
    """
type X = X of a : int* b: int
"""

  let goodTypeAnnotationArraySpacingTest =
    """
type X = X of a: int[]
"""

  let badTypeAnnotationArraySpacingTest =
    """
type X = X of a: int []
"""

  let goodParamArrayTest =
    """
let x (x: int[]) = x
"""

  let badParamArrayTest =
    """
let x (x: int []) = x
"""

  let goodTupleArrayTest =
    """
let x (x: int[] * int) = x
"""

  let badTupleArrayTest =
    """
let x (x: int[] * int []) = x
"""

  let goodArrowSpacingTest =
    """
let fn (x: list<int> -> string[] * string) = x
"""

  let badArrowSpacingTest =
    """
let fn (x: list<int>->string[] * string) = x
"""

  [<TestMethod>]
  member _.``Type Annotation Empty Paren Test``() =
    lint goodEmptyParenTest
    lintAssert badEmptyParenTest

  [<TestMethod>]
  member _.``Type Annotation Int Array Test``() =
    lint goodTypeAnnotationIntArrayTest
    lintAssert badTypeAnnotationIntArrayTest

  [<TestMethod>]
  member _.``Type Annotation(int) Test``() =
    lint goodTypeAnnotationIntTest
    lintAssert badTypeAnnotationIntTest

  [<TestMethod>]
  member _.``Type Annotation(string) Test``() =
    lint goodTypeAnnotationStringTest
    lintAssert badTypeAnnotationStringTest

  [<TestMethod>]
  member _.``Type Annotation(string) Space Test``() =
    lint goodTypeAnnotationStringSpaceTest
    lintAssert badTypeAnnotationStringSpaceTest

  [<TestMethod>]
  member _.``Type Annotation(Array) Test``() =
    lint goodTypeAnnotationArrayTest
    lintAssert badTypeAnnotationArrayTest

  [<TestMethod>]
  member _.``Type Annotation Colon Space Test``() =
    lint goodTypeAnnotationColonSpacingTest
    lintAssert badTypeAnnotationColonSpacingTest

  [<TestMethod>]
  member _.``Type Annotation Star Space Test``() =
    lint goodTypeAnnotationStarSpacingTest
    lintAssert badTypeAnnotationStarSpacingTest

  [<TestMethod>]
  member _.``Type Annotation Array Space Test``() =
    lint goodTypeAnnotationArraySpacingTest
    lintAssert badTypeAnnotationArraySpacingTest

  [<TestMethod>]
  member _.``Type Annotation Param Array Space Test``() =
    lint goodParamArrayTest
    lintAssert badParamArrayTest

  [<TestMethod>]
  member _.``Type Annotation Tuple Array Space Test``() =
    lint goodTupleArrayTest
    lintAssert badTupleArrayTest

  [<TestMethod>]
  member _.``Type Annotation Arrow Space Test``() =
    lint goodArrowSpacingTest
    lintAssert badArrowSpacingTest