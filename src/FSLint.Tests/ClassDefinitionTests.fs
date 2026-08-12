namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ClassDefinitionTests() =

  /// The constraints of a type parameter list are a separator list: `when`
  /// opens it and `and` divides it. Short enough, it keeps to one line.
  let goodInlineConstraintTest =
    """
type Small<'V, 'A when 'V: equality> = Small of 'V * 'A
"""

  /// Too long for one line, `when` opens a line of its own and each
  /// constraint takes one after it, the keywords standing in one column two
  /// spaces in from the declaration.
  let goodBrokenConstraintTest =
    """
type Good<'V, 'A
  when 'V :> IMonoid<'V>
  and 'V: (new: unit -> 'V)
  and 'A :> IMeasured<'V>> =
  | GoodCase of 'V
"""

  /// `when` left on the line the parameters are on, with the constraints
  /// chasing its column. The keywords no longer agree, and neither do the
  /// breaks: the first constraint shares a line while the rest do not.
  let badRaggedConstraintTest =
    """
type Ragged<'V, 'A when 'V :> IMonoid<'V>
                    and 'V: (new: unit -> 'V)
                    and 'A :> IMeasured<'V>> =
  | RaggedCase of 'V
"""

  /// How far in the keywords sit is left to the author, so long as they sit
  /// together.
  let goodDeeperConstraintTest =
    """
type Deeper<'V, 'A
      when 'V :> IMonoid<'V>
      and 'V: (new: unit -> 'V)
      and 'A :> IMeasured<'V>> =
  | DeeperCase of 'V
"""

  /// `when` opens its line, but an `and` under it wanders off the column.
  let badStrayConstraintTest =
    """
type Stray<'V, 'A
  when 'V :> IMonoid<'V>
  and 'V: (new: unit -> 'V)
      and 'A :> IMeasured<'V>> =
  | StrayCase of 'V
"""

  let goodImplicitCtorTest =
    """
type TestClass(param1: string, param2: int) =
  member _.Param1 = param1
"""

  let badImplicitCtorTest =
    """
type TestClass (param1: string, param2: int) =
  member _.Param1 = param1
"""

  let goodImplicitInheritTest =
    """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass(x: int, y: string) =
  inherit BaseClass(x)
  member _.Y = y
"""

  let badImplicitInheritTest =
    """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass(x: int, y: string) =
  inherit BaseClass (x)
  member _.Y = y
"""

  let goodExplicitInheritTest =
    """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass() =
  let helper = 42
  inherit BaseClass(helper)
  member _.Helper = helper
"""

  let badExplicitInheritTest =
    """
type BaseClass(value: int) =
  member _.Value = value

type DerivedClass() =
  let helper = 42
  inherit BaseClass (helper)
  member _.Helper = helper
"""

  let goodNestedInheritTest =
    """
type GrandParent(name: string) =
  member _.Name = name

type Parent(name: string, age: int) =
  inherit GrandParent(name)
  member _.Age = age

type Child(name: string, age: int, grade: int) =
  inherit Parent(name, age)
  member _.Grade = grade
"""

  let badNestedInheritTest =
    """
type GrandParent(name: string) =
  member _.Name = name

type Parent(name: string, age: int) =
  inherit GrandParent (name)
  member _.Age = age

type Child(name: string, age: int, grade: int) =
  inherit Parent (name, age)
  member _.Grade = grade
"""

  let goodMixedCaseTest =
    """
type ComplexClass(initialValue: int) =
  let mutable counter = 0
  inherit System.Object()
  member _.Value = initialValue
  member _.Increment() = counter <- counter + 1
"""

  let badMixedCaseTest =
    """
type ComplexClass (initialValue: int) =
  let mutable counter = 0
  inherit System.Object ()
  member _.Value = initialValue
  member _.Increment() = counter <- counter + 1
"""

  [<TestMethod>]
  member _.``[ClassDefinition] Constructor Parameter Spacing Test``() =
    lint goodImplicitCtorTest
    lintAssert badImplicitCtorTest

  [<TestMethod>]
  member _.``[ClassDefinition] Base Constructor Call Spacing Test``() =
    lint goodImplicitInheritTest
    lintAssert badImplicitInheritTest

  [<TestMethod>]
  member _.``[ClassDefinition] Explicit Base Class Call Spacing Test``() =
    lint goodExplicitInheritTest
    lintAssert badExplicitInheritTest

  [<TestMethod>]
  member _.``[ClassDefinition] Multiple Level Inheritance Spacing Test``() =
    lint goodNestedInheritTest
    lintAssert badNestedInheritTest

  [<TestMethod>]
  member _.``[ClassDefinition] Complex Class Definition Spacing Test``() =
    lint goodMixedCaseTest
    lintAssert badMixedCaseTest

  /// A constraint list that fits stays on its line; one that does not opens
  /// at `when`.
  [<TestMethod>]
  member _.``[ClassDefinition] Typar Constraint Placement Test``() =
    lint goodInlineConstraintTest
    lint goodBrokenConstraintTest
    lintAssertMsg "Move 'when' to the next line" badRaggedConstraintTest

  /// A `when` still up on the parameter line is the only thing said of such a
  /// list: sending it down takes the constraints below it along, so what the
  /// `and`s under it are doing cannot be judged until it lands.
  [<TestMethod>]
  member _.``[ClassDefinition] Typar Constraint Priority Test``() =
    lintErrors badRaggedConstraintTest
    |> fun errors ->
      Assert.AreEqual<int>(1, errors.Length)
      StringAssert.Contains(errors.Head.Message, "Move 'when' to the next line")

  /// Once `when` opens its line, every `and` stands in the column it opened.
  /// How far in that column falls is left to the author.
  [<TestMethod>]
  member _.``[ClassDefinition] Typar Constraint Alignment Test``() =
    lint goodDeeperConstraintTest
    lintAssertMsg "Align 'and' with 'when'" badStrayConstraintTest
