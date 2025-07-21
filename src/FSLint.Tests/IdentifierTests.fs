namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type IdentifierTests () =
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
  member _.``[ID] Binding Lowercase Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBindingLowercaseTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBindingLowercaseTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Binding PascalCase Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBindingPascalCaseTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBindingPascalCaseTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Record Definition PascalCase Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodRecordDefPascalCaseTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badRecordDefPascalCaseTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Record Field Name PascalCase Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodRecordFieldNamePascalCaseTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badRecordFieldNamePascalCaseTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[ID] Binding Underscore Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBindingUnderscoreTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBindingUnderscoreTest)
    ) |> ignore
