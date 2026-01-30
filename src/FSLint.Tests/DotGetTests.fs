namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type DotGetTests() =

  [<TestMethod>]
  member _.``[DotGet] Simple property access``() =
    let code = """let x = obj.Property"""
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[DotGet] Chained property access``() =
    let code = """let x = obj.Property.SubProperty.Value"""
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[DotGet] In array literal``() =
    let code = """let arr = [ obj.Prop1; obj.Prop2; obj.Prop3 ]"""
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[DotGet] In tuple``() =
    let code = """let t = (obj1.Prop, obj2.Prop, obj3.Prop)"""
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[DotGet] Static member access``() =
    let code =
      """
let x = System.String.Empty

let y = System.Console.WriteLine
"""
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (simple)``() =
    let code = """let x = obj .Property"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (chained)``() =
    let code = """let x = obj.Property .SubProperty"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (in array)``() =
    let code = """let arr = [ obj .Prop; other.Prop ]"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (in tuple)``() =
    let code = """let t = (obj .Prop1, obj.Prop2)"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error - space after dot (simple)``() =
    let code = """let x = obj. Property"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error - space after dot (chained)``() =
    let code = """let x = obj.Property. SubProperty"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error - space after dot (in array)``() =
    let code = """let lst = [ obj. Prop; other.Prop ]"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[DotGet] Error - spaces on both sides``() =
    let code = """let x = obj . Property"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore