namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type RecordTests() =

  let goodBracketPositionTest =
    """
type InsSize =
  { MemSize: MemorySize
    RegSize: RegType
    OperationSize: RegType
    SizeCond: OperandsSizeCondition }
"""

  let badBracketPositionTest =
    """
type InsSize =
  {
    MemSize: MemorySize
    RegSize: RegType
    OperationSize: RegType
    SizeCond: OperandsSizeCondition
  }
"""

  let badBracketPositionWithEqualTest =
    """
type InsSize = {
  MemSize: MemorySize
  RegSize: RegType
  OperationSize: RegType
  SizeCond: OperandsSizeCondition
  }
"""

  let badFieldTypeSpacingTest =
    """
type InsSize =
  { MemSize:  MemorySize
    RegSize:  RegType
    OperationSize:  RegType
    SizeCond:  OperandsSizeCondition }
"""

  let goodBracketSpacingTest =
    """
{ Prefixes = prefs }
"""

  let badBracketSpacingTest =
    """
{Prefixes = prefs}
"""

  let goodBracketSpacingMultiLineTest =
    """
{ Prefixes = prefs
  Opcode = opcode }
"""

  let badBracketSpacingMultiLineTest =
    """
{
  Prefixes = prefs
  Opcode = opcode
}
"""

  let badOperatorSpacingTest =
    """
{ field =value }
"""

  [<TestMethod>]
  member _.``[Record] Bracket Position Test``() =
    linterForFs.Lint(FakeFsPath, goodBracketPositionTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBracketPositionTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Bracket Position Inline With Equal Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBracketPositionWithEqualTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Field Type Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badFieldTypeSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Bracket Spacing Test``() =
    linterForFs.Lint(FakeFsPath, goodBracketSpacingTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBracketSpacingTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Bracket Spacing MultiLine Test``() =
    linterForFs.Lint(FakeFsPath, goodBracketSpacingMultiLineTest)
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badBracketSpacingMultiLineTest)
    ) |> ignore

  [<TestMethod>]
  member _.``[Record] Operator Spacing Test``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, badOperatorSpacingTest)
    ) |> ignore