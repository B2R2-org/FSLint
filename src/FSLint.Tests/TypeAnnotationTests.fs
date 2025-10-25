namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TypeAnnotationTest() =
  let goodEmptyParenTest = """
let fn () = ()
"""

  let badEmptyParenTest = """
let fn ( ) = ()
"""

  let goodTypeAnnotationIntArrayTest = """
let fn (param: int[]) = 10
"""

  let badTypeAnnotationIntArrayTest = """
let fn (param: int []) = 10
"""

  let goodTypeAnnotationIntTest = """
let fn (p: int) = 10
"""

  let badTypeAnnotationIntTest = """
let fn (p:int) = 10
"""

  let goodTypeAnnotationStringTest = """
let fn (str: string) = "10"
"""

  let badTypeAnnotationStringTest = """
let fn (str:string) = "10"
"""

  let goodTypeAnnotationStringSpaceTest = """
let fn (str: string) = "10"
"""

  let badTypeAnnotationStringSpaceTest = """
let fn (str :string) = "10"
"""

  let goodTypeAnnotationArrayTest = """
let inline toString (stmts: LowUIR.Stmt[]) = ()
"""

  let badTypeAnnotationArrayTest = """
let inline toString (stmts:  LowUIR.Stmt[]) = ()
"""

  let goodTypeAnnotationColonSpacingTest = """
type X = X of a: int * b: int
"""

  let badTypeAnnotationColonSpacingTest = """
type X = X of a : int * b: int
"""

  let goodTypeAnnotationStarSpacingTest = """
type X = X of a: int * b: int
"""

  let badTypeAnnotationStarSpacingTest = """
type X = X of a : int* b: int
"""

  let goodTypeAnnotationArraySpacingTest = """
type X = X of a: int[]
"""

  let badTypeAnnotationArraySpacingTest = """
type X = X of a: int []
"""

  let goodParamArrayTest = """
let x (x: int[]) = x
"""

  let badParamArrayTest = """
let x (x: int []) = x
"""

  let goodTupleArrayTest = """
let x (x: int[] * int) = x
"""

  let badTupleArrayTest = """
let x (x: int[] * int []) = x
"""

  let goodArrowSpacingTest = """
let fn (x: list<int> -> string[] * string) = x
"""

  let badArrowSpacingTest = """
let fn (x: list<int>->string[] * string) = x
"""

  [<TestMethod>]
  member _.``Type Annotation Empty Paren Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodEmptyParenTest
    assertFSLintFailure Constants.FakeFsPath badEmptyParenTest

  [<TestMethod>]
  member _.``Type Annotation Int Array Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationIntArrayTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationIntArrayTest

  [<TestMethod>]
  member _.``Type Annotation(int) Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationIntTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationIntTest

  [<TestMethod>]
  member _.``Type Annotation(string) Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationStringTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationStringTest

  [<TestMethod>]
  member _.``Type Annotation(string) Space Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationStringSpaceTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationStringSpaceTest

  [<TestMethod>]
  member _.``Type Annotation(Array) Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationArrayTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationArrayTest

  [<TestMethod>]
  member _.``Type Annotation Colon Space Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationColonSpacingTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationColonSpacingTest

  [<TestMethod>]
  member _.``Type Annotation Star Space Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationStarSpacingTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationStarSpacingTest

  [<TestMethod>]
  member _.``Type Annotation Array Space Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTypeAnnotationArraySpacingTest
    assertFSLintFailure Constants.FakeFsPath badTypeAnnotationArraySpacingTest

  [<TestMethod>]
  member _.``Type Annotation Param Array Space Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodParamArrayTest
    assertFSLintFailure Constants.FakeFsPath badParamArrayTest

  [<TestMethod>]
  member _.``Type Annotation Tuple Array Space Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodTupleArrayTest
    assertFSLintFailure Constants.FakeFsPath badTupleArrayTest

  [<TestMethod>]
  member _.``Type Annotation Arrow Space Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodArrowSpacingTest
    assertFSLintFailure Constants.FakeFsPath badArrowSpacingTest