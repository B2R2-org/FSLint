namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type AppTests() =

  let goodListSpaceInfixTest = """
[ this.Address + uint64 this.Length + this.Name ]
"""

  let badListSpaceInfixTest = """
[ this.Address +  uint64 this.Length + this.Name ]
"""

  let goodListAppIndexerTest = """
[ lifter.File.RawBytes[ptr.Offset] ]
"""

  let badListAppIndexerTest = """
[ lifter.File.RawBytes[ ptr.Offset ] ]
"""

  let goodListAppIndexerInRangeTest = """
good[1..]
"""

  let badListAppIndexerInRangeTest = """
bad[ 1.. ]
"""

  let goodListSpaceFunAppTest = """[ fn 1 2 3 x ]"""

  let badListSpaceFunAppTest = """[ fn  1 2 3 x ]"""

  [<TestMethod>]
  member _.``[App] List Space Before and After Infix Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodListSpaceInfixTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badListSpaceInfixTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[App] List App Indexer Bracket Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodListAppIndexerTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badListAppIndexerTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[App] List App Indexer Index Range Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodListAppIndexerInRangeTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badListAppIndexerInRangeTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[App] List Space Fununction Application Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodListSpaceFunAppTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badListSpaceFunAppTest)
    ) |> ignore
