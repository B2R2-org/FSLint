namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type TypeAnnotationTest () =
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

  [<TestMethod>]
  member _.``Type Annotation(int) Test``() =
    linterForFs.Lint
      Constants.FakeFsPath
      goodTypeAnnotationIntTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint
        Constants.FakeFsPath
        badTypeAnnotationIntTest
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation(string) Test``() =
    linterForFs.Lint
      Constants.FakeFsPath
      goodTypeAnnotationStringTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint
        Constants.FakeFsPath
        badTypeAnnotationStringTest
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation(string) Space Test``() =
    linterForFs.Lint
      Constants.FakeFsPath
      goodTypeAnnotationStringSpaceTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint
        Constants.FakeFsPath
        badTypeAnnotationStringSpaceTest
    ) |> ignore

  [<TestMethod>]
  member _.``Type Annotation(Array) Test``() =
    linterForFs.Lint
      Constants.FakeFsPath
      goodTypeAnnotationArrayTest
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint
        Constants.FakeFsPath
        badTypeAnnotationArrayTest
    ) |> ignore
