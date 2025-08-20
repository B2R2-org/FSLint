namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

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
    linterForFs.Lint(Constants.FakeFsPath, goodEmptyParenTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badEmptyParenTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation Int Array Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationIntArrayTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationIntArrayTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation(int) Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationIntTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationIntTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation(string) Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationStringTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationStringTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation(string) Space Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationStringSpaceTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationStringSpaceTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation(Array) Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationArrayTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationArrayTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation Colon Space Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationColonSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationColonSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation Star Space Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationStarSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationStarSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation Array Space Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTypeAnnotationArraySpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTypeAnnotationArraySpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation Param Array Space Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodParamArrayTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badParamArrayTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation Tuple Array Space Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodTupleArrayTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badTupleArrayTest)
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation Arrow Space Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodArrowSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badArrowSpacingTest)
    ) |> ignore