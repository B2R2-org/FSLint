namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

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
    assertFSLintSuccess Constants.FakeFsPath goodListSpaceInfixTest
    assertFSLintFailure Constants.FakeFsPath badListSpaceInfixTest

  [<TestMethod>]
  member _.``[App] List App Indexer Bracket Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodListAppIndexerTest
    assertFSLintFailure Constants.FakeFsPath badListAppIndexerTest

  [<TestMethod>]
  member _.``[App] List App Indexer Index Range Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodListAppIndexerInRangeTest
    assertFSLintFailure Constants.FakeFsPath badListAppIndexerInRangeTest

  [<TestMethod>]
  member _.``[App] List Space Fununction Application Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodListSpaceFunAppTest
    assertFSLintFailure Constants.FakeFsPath badListSpaceFunAppTest
