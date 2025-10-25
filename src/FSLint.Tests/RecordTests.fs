namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type RecordTests() =

  let goodBracketPositionTest = """
type InsSize =
  { MemSize: MemorySize
    RegSize: RegType
    OperationSize: RegType
    SizeCond: OperandsSizeCondition }
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

  let badBracketPositionWithEqualTest = """
type InsSize = {
  MemSize: MemorySize
  RegSize: RegType
  OperationSize: RegType
  SizeCond: OperandsSizeCondition
  }
"""

  let badFieldTypeSpacingTest = """
type InsSize =
  { MemSize:  MemorySize
    RegSize:  RegType
    OperationSize:  RegType
    SizeCond:  OperandsSizeCondition }
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
    assertFSLintSuccess Constants.FakeFsPath goodBracketPositionTest
    assertFSLintFailure Constants.FakeFsPath badBracketPositionTest

  [<TestMethod>]
  member _.``[Record] Bracket Position Inline With Equal Test``() =
    assertFSLintFailure Constants.FakeFsPath badBracketPositionWithEqualTest

  [<TestMethod>]
  member _.``[Record] Field Type Spacing Test``() =
    assertFSLintFailure Constants.FakeFsPath badFieldTypeSpacingTest

  [<TestMethod>]
  member _.``[Record] Bracket Spacing Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodBracketSpacingTest
    assertFSLintFailure Constants.FakeFsPath badBracketSpacingTest

  [<TestMethod>]
  member _.``[Record] Bracket Spacing MultiLine Test``() =
    assertFSLintSuccess Constants.FakeFsPath goodBracketSpacingMultiLineTest
    assertFSLintFailure Constants.FakeFsPath badBracketSpacingMultiLineTest

  [<TestMethod>]
  member _.``[Record] Operator Spacing Test``() =
    assertFSLintFailure Constants.FakeFsPath badOperatorSpacingTest