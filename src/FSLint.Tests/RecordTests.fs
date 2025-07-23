namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type RecordTests () =

  let goodBracketPositionTest = """
type InsSize = {
  MemSize: MemorySize
  RegSize: RegType
  OperationSize: RegType
  SizeCond: OperandsSizeCondition
}
"""

  let badBracketPositionTest = """
type InsSize =
  {
    MemSize: MemorySize
    RegSize: RegType
    OperationSize: RegType
    SizeCond: OperandsSizeCondition
  }
"""

  let badFieldTypeSpacingTest = """
type InsSize = {
  MemSize:  MemorySize
  RegSize:  RegType
  OperationSize:  RegType
  SizeCond:  OperandsSizeCondition
}
"""

  let goodBracketSpacingTest = """
{ Prefixes = prefs }
"""

  let badBracketSpacingTest = """
{Prefixes = prefs}
"""

  let goodBracketSpacingMultiLineTest = """
{ Prefixes = prefs
  Opcode = opcode }
"""

  let badBracketSpacingMultiLineTest = """
{
  Prefixes = prefs
  Opcode = opcode
}
"""

  let badOperatorSpacingTest = """
{ field =value }
"""

  [<TestMethod>]
  member _.``[Record] Bracket Position Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBracketPositionTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBracketPositionTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Field Type Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badFieldTypeSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Bracket Spacing Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBracketSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Bracket Spacing MultiLine Test``() =
    linterForFs.Lint(Constants.FakeFsPath, goodBracketSpacingMultiLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badBracketSpacingMultiLineTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Operator Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badOperatorSpacingTest)
    ) |> ignore