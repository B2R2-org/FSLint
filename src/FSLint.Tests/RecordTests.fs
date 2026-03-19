namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

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

  let goodAnonRecdTest =
    """
let good = {| A = 1; B = 1 |}
"""

  let goodAnonRecdCopyTest =
    """
let good = {| original with Age = 31; Name = "Bob" |}
"""

  let badAnonRecdTest =
    """
let bad = {| A=1; B = 1 |}
"""

  let badAnonRecdCopyTest =
    """
let bad = {| original with Age = 31; Name="Bob" |}
"""

  [<TestMethod>]
  member _.``[Record] Bracket Position Test``() =
    lint goodBracketPositionTest
    lintAssert badBracketPositionTest

  [<TestMethod>]
  member _.``[Record] Bracket Position Inline With Equal Test``() =
    lintAssert badBracketPositionWithEqualTest

  [<TestMethod>]
  member _.``[Record] Field Type Spacing Test``() =
    lintAssert badFieldTypeSpacingTest

  [<TestMethod>]
  member _.``[Record] Bracket Spacing Test``() =
    lint goodBracketSpacingTest
    lintAssert badBracketSpacingTest

  [<TestMethod>]
  member _.``[Record] Bracket Spacing MultiLine Test``() =
    lint goodBracketSpacingMultiLineTest
    lintAssert badBracketSpacingMultiLineTest

  [<TestMethod>]
  member _.``[Record] Operator Spacing Test``() =
    lintAssert badOperatorSpacingTest

  [<TestMethod>]
  member _.``[Record] Anonymous Operator Spacing Test``() =
    lint goodAnonRecdTest
    lint goodAnonRecdCopyTest
    lintAssert badAnonRecdTest
    lintAssert badAnonRecdCopyTest