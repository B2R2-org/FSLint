namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type IdentifierTest () =
  let goodBindingLowercaseTest = """
let age = 30
"""

  let badBindingLowercaseTest = """
let Age = 30
"""

  let goodBindingPascalCaseTest = """
let [<Literal>] Age = 30
"""

  let badBindingPascalCaseTest = """
let [<Literal>] age = 30
"""

  let goodRecordDefPascalCaseTest = """
type Person = { Age: int }
"""

  let badRecordDefPascalCaseTest = """
type person = { Age: int }
"""

  let goodRecordFieldNamePascalCaseTest = """
type Person = { Age: int }
"""

  let badRecordFieldNamePascalCaseTest = """
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
      Constants.FakeFsPath
      goodBindingLowercaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        Constants.FakeFsPath
        badBindingLowercaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Binding PascalCase Test`` () =
    lintTextString
      Constants.FakeFsPath
      goodBindingPascalCaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        Constants.FakeFsPath
        badBindingPascalCaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Record Definition PascalCase Test`` () =
    lintTextString
      Constants.FakeFsPath
      goodRecordDefPascalCaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        Constants.FakeFsPath
        badRecordDefPascalCaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Record Field Name PascalCase Test`` () =
    lintTextString
      Constants.FakeFsPath
      goodRecordFieldNamePascalCaseTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        Constants.FakeFsPath
        badRecordFieldNamePascalCaseTest
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Binding Underscore Test`` () =
    lintTextString
      Constants.FakeFsPath
      goodBindingUnderscoreTest
    Assert.ThrowsException<LintException> (fun () ->
      lintTextString
        Constants.FakeFsPath
        badBindingUnderscoreTest
    ) |> ignore
