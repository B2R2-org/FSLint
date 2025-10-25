namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ClassDefinitionTests() =

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

  [<TestMethod>]
  member _.``[ClassDefinition] Constructor Parameter Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodImplicitCtorTest
    assertFSLintFailure Constants.FakeFsPath badImplicitCtorTest

  [<TestMethod>]
  member _.``[ClassDefinition] Base Constructor Call Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodImplicitInheritTest
    assertFSLintFailure Constants.FakeFsPath badImplicitInheritTest

  [<TestMethod>]
  member _.``[ClassDefinition] Explicit Base Class Call Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodExplicitInheritTest
    assertFSLintFailure Constants.FakeFsPath badExplicitInheritTest

  [<TestMethod>]
  member _.``[ClassDefinition] Multiple Level Inheritance Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodNestedInheritTest
    assertFSLintFailure Constants.FakeFsPath badNestedInheritTest

  [<TestMethod>]
  member _.``[ClassDefinition] Complex Class Definition Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodMixedCaseTest
    assertFSLintFailure Constants.FakeFsPath badMixedCaseTest