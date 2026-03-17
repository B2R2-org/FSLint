namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type IdentifierTests() =
  let goodBindingLowercaseTest =
    """
let age = 30
"""

  let badBindingLowercaseTest =
    """
let Age = 30
"""

  let goodBindingPascalCaseTest =
    """
let [<Literal>] Age = 30
"""

  let badBindingPascalCaseTest =
    """
let [<Literal>] age = 30
"""

  let goodRecordDefPascalCaseTest =
    """
type Person = { Age: int }
"""

  let badRecordDefPascalCaseTest =
    """
type person = { Age: int }
"""

  let goodRecordFieldNamePascalCaseTest =
    """
type Person = { Age: int }
"""

  let badRecordFieldNamePascalCaseTest =
    """
type Person = { age: int }
"""

  let goodBindingUnderscoreTest =
    """
let _age = 30
"""

  let badBindingUnderscoreTest =
    """
let age_ = 30
"""

  [<TestMethod>]
  member _.``[ID] Binding Lowercase Test``() =
    lint goodBindingLowercaseTest
    lintAssert badBindingLowercaseTest

  [<TestMethod>]
  member _.``[ID] Binding PascalCase Test``() =
    lint goodBindingPascalCaseTest
    lintAssert badBindingPascalCaseTest

  [<TestMethod>]
  member _.``[ID] Record Definition PascalCase Test``() =
    lint goodRecordDefPascalCaseTest
    lintAssert badRecordDefPascalCaseTest

  [<TestMethod>]
  member _.``[ID] Record Field Name PascalCase Test``() =
    lint goodRecordFieldNamePascalCaseTest
    lintAssert badRecordFieldNamePascalCaseTest

  [<TestMethod>]
  member _.``[ID] Binding Underscore Test``() =
    lint goodBindingUnderscoreTest
    lintAssert badBindingUnderscoreTest
