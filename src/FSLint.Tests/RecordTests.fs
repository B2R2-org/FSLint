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

  let badAnonRecdLeftBracketSpacingTest =
    """
let bad = {|A = 1; B = 1 |}
"""

  let badAnonRecdRightBracketSpacingTest =
    """
let bad = {| A = 1; B = 1|}
"""

  let badAnonRecdCopyLeftBracketSpacingTest =
    """
let bad = {|original with Age = 31; Name = "Bob" |}
"""

  let badAnonRecdCopyRightBracketSpacingTest =
    """
let bad = {| original with Age = 31; Name = "Bob"|}
"""

  let goodFieldPerLineTest =
    """
type Point =
  { X: int
    Y: int
    Z: int }

let origin =
  { X = 42
    Y = 42
    Z = 42 }
"""

  /// A record standing on one line has chosen no layout to answer for.
  let goodFieldsOnOneLineTest =
    """
type Point =
  { X: int
    Y: int
    Z: int }

let origin = { X = 42; Y = 42; Z = 42 }
"""

  let badFieldPerLineTest =
    """
type Point =
  { X: int
    Y: int
    Z: int }

let origin =
  { X = 42; Y = 42
    Z = 42 }
"""

  let badFieldsSharingALineTest =
    """
type Point =
  { X: int
    Y: int
    Z: int
    W: int }

let origin =
  { X = 42; Y = 42; Z = 42
    W = 42 }
"""

  let badCopyFieldPerLineTest =
    """
type Point =
  { X: int
    Y: int
    Z: int }

let move (p: Point) =
  { p with X = 42; Y = 42
           Z = 42 }
"""

  let goodAnonFieldPerLineTest =
    """
let origin =
  {| X = 42
     Y = 42
     Z = 42 |}
"""

  let goodAnonFieldsOnOneLineTest =
    """
let origin = {| X = 42; Y = 42; Z = 42 |}
"""

  let badAnonFieldPerLineTest =
    """
let origin =
  {| X = 42; Y = 42
     Z = 42 |}
"""

  let badAnonCopyFieldPerLineTest =
    """
let move p =
  {| p with X = 42; Y = 42
            Z = 42 |}
"""

  let goodRecordDefinitionOnOneLineTest =
    """
type Point = { X: int; Y: int; Z: int }
"""

  let badUnionFieldStarTest =
    """
type Shape =
  | Rect of int*int
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

  [<TestMethod>]
  member _.``[Record] Anonymous Bracket Spacing Test``() =
    lintAssert badAnonRecdLeftBracketSpacingTest
    lintAssert badAnonRecdRightBracketSpacingTest
    lintAssert badAnonRecdCopyLeftBracketSpacingTest
    lintAssert badAnonRecdCopyRightBracketSpacingTest

  /// Every field of a record spread down the page begins a line of its own,
  /// and where several share a line it is the ones after the first that are
  /// named. A record still on one line is asked nothing: what is wrong with a
  /// line too long is its length, and the budget says so.
  [<TestMethod>]
  member _.``[Record] Field Per Line Test``() =
    lint goodFieldPerLineTest
    lint goodFieldsOnOneLineTest
    lintAssertMsg "Use one element per line" badFieldPerLineTest
    lintAssertMsg "Use one element per line" badCopyFieldPerLineTest

  /// An anonymous record keeps its field names in a different node of the
  /// tree, and answers the same way for all that.
  [<TestMethod>]
  member _.``[Record] Anonymous Field Per Line Test``() =
    lint goodAnonFieldPerLineTest
    lint goodAnonFieldsOnOneLineTest
    lintAssertMsg "Use one element per line" badAnonFieldPerLineTest
    lintAssertMsg "Use one element per line" badAnonCopyFieldPerLineTest

  /// The fields of a union case are divided by `*` and answer for the spacing
  /// round it. A record's are divided by `;`, and asking that gap to read
  /// ` * ` would have the author write a tuple where they wrote a record.
  ///
  /// The union half is read through `lintErrors` rather than asserted on:
  /// `checkFieldsWidth` wraps itself in a `try`, which swallows the exception
  /// a context-free lint raises and leaves nothing for `lintAssertMsg` to
  /// catch. Given a context the report is recorded rather than raised.
  [<TestMethod>]
  member _.``[Record] Definition Separator Test``() =
    lint goodRecordDefinitionOnOneLineTest
    lintErrors badUnionFieldStarTest
    |> List.filter (fun e -> e.Message = "Use ' * '")
    |> fun errors -> Assert.AreEqual<int>(1, errors.Length)

  /// Two fields following the first on its line are two reports, so lifting
  /// one leaves the other still named.
  [<TestMethod>]
  member _.``[Record] Field Per Line Test(2)``() =
    lintErrors badFieldsSharingALineTest
    |> List.filter (fun e -> e.Message = "Use one element per line")
    |> fun errors -> Assert.AreEqual<int>(2, errors.Length)
