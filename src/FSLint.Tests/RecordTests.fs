namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type RecordTests() =

  /// Which layout a record definition is written in is left to whoever writes
  /// it: braces beside the fields, braces on lines of their own, or the
  /// opening one left up beside the `=`. What is read is the space between a
  /// brace and the field it fences, and only where the two share a line.
  let goodBracketPositionTest =
    """
type InsSize =
  { MemSize: MemorySize
    RegSize: RegType
    OperationSize: RegType
    SizeCond: OperandsSizeCondition }
"""

  /// A brace alone on a row below the `=` is neither layout: nothing shares
  /// its row, so it reads as a row that lost whatever belonged on it.
  let badBracesOnOwnLinesTest =
    """
type InsSize =
  {
    MemSize: MemorySize
    RegSize: RegType
  }
"""

  let goodBraceBesideEqualTest =
    """
type InsSize = {
  MemSize: MemorySize
  RegSize: RegType
}
"""

  /// The range the parser gives a record definition opens at its access
  /// modifier and not at its brace. Read as the brace, every column below it
  /// comes out short by the width of the modifier, and a record spaced exactly
  /// right is reported for it -- with the caret on `private`, at that.
  let goodPrivateBesideEqualTest =
    """
type Foo = private {
  A: int
  B: int
}
"""

  let goodPrivateOnOwnLineTest =
    """
type Foo =
  private
    { A: int
      B: int }
"""

  let goodPrivateWithBraceTest =
    """
type Foo =
  private {
    A: int
    B: int
  }
"""

  let goodPrivateInlineTest =
    """
type Foo = private { A: int }
"""

  /// A brace alone on a row is neither layout whether a modifier stands above
  /// it or not: what decides is that nothing shares the brace's own row.
  let badPrivateBraceAloneTest =
    """
type Foo =
  private
    {
      A: int
      B: int
    }
"""

  let badPrivateLeftSpacingTest =
    """
type Foo = private {A: int }
"""

  let badPrivateRightSpacingTest =
    """
type Foo = private { A: int}
"""

  /// The two braces are read as a pair. Either both stand beside the fields
  /// they fence or both stand on lines of their own; a definition taking the
  /// top from one layout and the bottom from the other reads as a line gone
  /// missing, and each of the three accepted layouts has such a twin.
  let badBraceOpenAloneTest =
    """
type Foo = private {
  A: int
  B: int }
"""

  let badBraceCloseAloneTest =
    """
type Foo =
  private
    { A: int
      B: int
    }
"""

  let badBraceCloseTightTest =
    """
type Foo =
  private {
    A: int
    B: int}
"""

  /// Nothing about this turns on the modifier: it is where the braces went.
  let badBraceNoModifierTest =
    """
type Foo = {
  A: int
  B: int }
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
    lint goodBraceBesideEqualTest
    lintAssertMsg "Use consistent bracket placement"
      badBracesOnOwnLinesTest

  /// The space beside a brace is still read once a modifier stands in front of
  /// it, and it is read from the brace.
  [<TestMethod>]
  member _.``[Record] Access Modifier Brace Test``() =
    lint goodPrivateBesideEqualTest
    lint goodPrivateOnOwnLineTest
    lint goodPrivateWithBraceTest
    lint goodPrivateInlineTest
    lintAssertMsg "Use single whitespace after '{'" badPrivateLeftSpacingTest
    lintAssertMsg "Use single whitespace before '}'" badPrivateRightSpacingTest
    lintAssertMsg "Use consistent bracket placement" badPrivateBraceAloneTest

  /// Each accepted layout paired with the twin that mixes it with the other.
  [<TestMethod>]
  member _.``[Record] Brace Agreement Test``() =
    lintAssertMsg "Use consistent bracket placement" badBraceOpenAloneTest
    lintAssertMsg "Use consistent bracket placement" badBraceCloseAloneTest
    lintAssertMsg "Use consistent bracket placement" badBraceCloseTightTest
    lintAssertMsg "Use consistent bracket placement" badBraceNoModifierTest

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
  [<TestMethod>]
  member _.``[Record] Definition Separator Test``() =
    lint goodRecordDefinitionOnOneLineTest
    lintAssertMsg "Use ' * '" badUnionFieldStarTest

  /// Two fields following the first on its line are two reports, so lifting
  /// one leaves the other still named.
  [<TestMethod>]
  member _.``[Record] Field Per Line Test(2)``() =
    lintErrors badFieldsSharingALineTest
    |> List.filter (fun e -> e.Message = "Use one element per line")
    |> fun errors -> Assert.AreEqual<int>(2, errors.Length)

  /// A comment keeping a brace company counts as company. A comment is prose
  /// and its place is the author's business, so a note left in front of a
  /// closing brace must not make a record that agrees with itself look as
  /// though it did not. A note on a row of its own keeps nobody company, and
  /// the record is read as it would be without the note.
  [<TestMethod>]
  member _.``[Record] Brace Symmetry Reads A Comment As Company``() =
    lint "type T =\n  { A: int\n    B: int\n    (* note *) }\n"
    lint "type T =\n  { (* note *)\n    A: int\n    B: int }\n"
    lintAssertMsg "Use consistent bracket placement"
      "type T =\n  { A: int\n    B: int\n    (* note *)\n  }\n"

  /// The gap between a field's name and its type is read where those two
  /// stand. A type running onto a second row leaves the field ending well
  /// below its own name, and reading the gap on that row reads some other part
  /// of the field.
  [<TestMethod>]
  member _.``[Record] Field Colon Of A Multiline Type Test``() =
    lint "type T =\n  { A: Map<string,\n           int> }\n"
    lintAssertMsg "Use ': ' between field and type"
      "type T =\n  { A: int\n    B:  Map<string,\n            int> }\n"

  /// One gap, one finding. A record field was answering for the colon twice,
  /// once here and once where the type annotation of a field is read.
  [<TestMethod>]
  member _.``[Record] Field Colon Is Reported Once Test``() =
    lintErrors "type T =\n  { A:int }\n"
    |> List.filter (fun e -> e.Message.Contains "': '")
    |> fun errors -> Assert.AreEqual<int>(1, errors.Length)
