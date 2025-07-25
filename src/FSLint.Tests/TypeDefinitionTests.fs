namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type TypeDefinitionTests() =

  let goodImplicitCtorTest = """
type TestClass(param1: string, param2: int) =
  member _.Param1 = param1
"""

  let badImplicitCtorTest = """
type TestClass (param1: string, param2: int) =
  member _.Param1 = param1
"""

  let goodImplicitInheritTest = """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass(x: int, y: string) =
  inherit BaseClass(x)
  member _.Y = y
"""

  let badImplicitInheritTest = """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass(x: int, y: string) =
  inherit BaseClass (x)
  member _.Y = y
"""

  let goodExplicitInheritTest = """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass() =
  let helper = 42
  inherit BaseClass(helper)
  member _.Helper = helper
"""

  let badExplicitInheritTest = """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass() =
  let helper = 42
  inherit BaseClass (helper)
  member _.Helper = helper
"""

  let goodNestedInheritTest = """
type GrandParent(name: string) =
  member _.Name = name

type Parent(name: string, age: int) =
  inherit GrandParent(name)
  member _.Age = age

type Child(name: string, age: int, grade: int) =
  inherit Parent(name, age)
  member _.Grade = grade
"""

  let badNestedInheritTest = """
type GrandParent(name: string) =
  member _.Name = name

type Parent(name: string, age: int) =
  inherit GrandParent (name)
  member _.Age = age

type Child(name: string, age: int, grade: int) =
  inherit Parent (name, age)
  member _.Grade = grade
"""

  let goodMixedCaseTest = """
type ComplexClass(initialValue: int) =
  let mutable counter = 0
  inherit System.Object()
  member _.Value = initialValue
  member _.Increment() = counter <- counter + 1
"""

  let badMixedCaseTest = """
type ComplexClass (initialValue: int) =
  let mutable counter = 0
  inherit System.Object ()
  member _.Value = initialValue
  member _.Increment() = counter <- counter + 1
"""

  let goodRecordTypeNewLineTest = """
type Person =
  { Name: string
    Age: int
    Email: string }
"""

  let badRecordTypeNewLineTest = """
type Person =
  { Name: string

    Age: int
    Email: string }
"""

  let goodEnumTypeNewLineTest = """
type Direction =
  | North = 0
  | South = 1
  | East = 2
  | West = 3
"""

  let badEnumTypeNewLineTest = """
type Direction =
  | North = 0

  | South = 1
  | East = 2
  | West = 3
"""

  let goodDUTypeNewLineTest = """
type Color =
  | Red
  | Green
  | Blue
"""

  let badDUTypeNewLineTest = """
type Color =
  | Red

  | Green
  | Blue
"""

  [<TestMethod>]
  member _.``[TypeDefn] Constructor Parameter Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodImplicitCtorTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badImplicitCtorTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[TypeDefn] Base Constructor Call Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodImplicitInheritTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badImplicitInheritTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[TypeDefn] Explicit Base Class Call Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodExplicitInheritTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badExplicitInheritTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[TypeDefn] Multiple Level Inheritance Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodNestedInheritTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badNestedInheritTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[TypeDefn] Complex Class Definition Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodMixedCaseTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badMixedCaseTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[TypeDefn] Record Type NewLine Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodRecordTypeNewLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badRecordTypeNewLineTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[TypeDefn] Enum Type NewLine Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodEnumTypeNewLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badEnumTypeNewLineTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[TypeDefn] Discriminated Union NewLine Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodDUTypeNewLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badDUTypeNewLineTest)
    ) |> ignore