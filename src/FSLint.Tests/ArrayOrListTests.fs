namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type ArrayOrListTests () =
  let goodArray = "[| 1; 2; 3; 4 |]"
  let badArray = "[|1; 2; 3; 4|]"

  [<TestMethod>]
  member _.``[ArrayOrList] Test`` () =
    linterForFs.Lint Constants.FakeFsPath goodArray
    Assert.ThrowsException<LintException> (fun () ->
      linterForFs.Lint Constants.FakeFsPath badArray
    ) |> ignore
