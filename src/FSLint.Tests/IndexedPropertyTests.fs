namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type IndexedPropertyTests () =

  let goodIndexedPropertyTest = """src[1]"""

  let badIndexedPropertySpacingFromAppTest = """src [1]"""

  let badIndexedPropertyBracketSpacingTest = """src[ 1 ]"""

  let goodIndexedPropertyHasOpmTest = """src[expr1..opm..expr2]"""

  let badIndexedPropertyHasOpmTest = """src[expr1 .. opm .. expr2]"""

  [<TestMethod>]
  member _.``[IndexedProperty] Indexed Property Spacing From App Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodIndexedPropertyTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badIndexedPropertySpacingFromAppTest
    ) |> ignore

  [<TestMethod>]
  member _.``[IndexedProperty] Indexed Property Bracket Spacing Test`` () =
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badIndexedPropertyBracketSpacingTest
    ) |> ignore

  [<TestMethod>]
  member _.``[IndexedProperty] Indexed Property Range Operator Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodIndexedPropertyHasOpmTest
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badIndexedPropertyHasOpmTest
    ) |> ignore