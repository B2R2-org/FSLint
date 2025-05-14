namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type IdentifierTest () =

  let fakePath = "IdentifierTest.fs"

  let goodBindingLowercaseTest = """
let age = 30
"""

  let badBindingLowercaseTest = """
let Age = 30
"""

  let goodBindingPascalcaseTest = """
let [<Literal>] Age = 30
"""

  let badBindingPascalcaseTest = """
let [<Literal>] age = 30
"""

  let goodRecordDefPascalcaseTest = """
type Person = { Age: int }
"""

  let badRecordDefPascalcaseTest = """
type person = { Age: int }
"""

  let goodRecordFieldNamePascalcaseTest = """
type Person = { Age: int }
"""

  let badRecordFieldNamePascalcaseTest = """
type Person = { age: int }
"""

  let goodBindingUnderscoreTest = """
let _age = 30
"""

  let badBindingUnderscoreTest = """
let age_ = 30
"""

  [<TestMethod>]
  member _.``[ID] Binding Lowercase Test`` () =
    lintTextString
      fakePath
      goodBindingLowercaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        fakePath
        badBindingLowercaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Binding Pascalcase Test`` () =
    lintTextString
      fakePath
      goodBindingPascalcaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        fakePath
        badBindingPascalcaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Record Definition Pascalcase Test`` () =
    lintTextString
      fakePath
      goodRecordDefPascalcaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        fakePath
        badRecordDefPascalcaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Record Field Name Pascalcase Test`` () =
    lintTextString
      fakePath
      goodRecordFieldNamePascalcaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        fakePath
        badRecordFieldNamePascalcaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Binding Underscore Test`` () =
    lintTextString
      fakePath
      goodBindingUnderscoreTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        fakePath
        badBindingUnderscoreTest
    ) |> ignore
