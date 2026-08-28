namespace B2R2.FSLint

open System.Collections.Generic
open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTrivia

type CaseStyle =
  | LowerCamelCase
  | PascalCase

/// A stretch reduced to the four numbers that tell it from every other: the
/// line and column it opens on, and the line and column it closes on. A store
/// keyed by this answers whether it holds a stretch without walking what it
/// holds.
type RangeKey = System.ValueTuple<int, int, int, int>

/// The file's text, with the lines it has already been asked for kept.
///
/// Nearly every rule reads the line a range sits on, and the rules do not
/// share what they read: one line is cut out of the text once per rule that
/// wants it. Cutting it once and keeping it costs one array the length of the
/// file, and the file is being held in memory either way.
///
/// Everything else is handed straight to the text underneath.
type CachingSourceText(inner: ISourceText) =
  let lines: string array = Array.zeroCreate (inner.GetLineCount())

  interface ISourceText with
    member _.Item with get index = inner[index]

    member _.Length = inner.Length

    member _.GetLineString index =
      if index < 0 || index >= lines.Length then
        inner.GetLineString index
      else
        match lines[index] with
        | null ->
          let line = inner.GetLineString index
          lines[index] <- line
          line
        | line ->
          line

    member _.GetLineCount() = inner.GetLineCount()

    member _.GetLastCharacterPosition() = inner.GetLastCharacterPosition()

    member _.GetSubTextString(start, length) =
      inner.GetSubTextString(start, length)

    member _.SubTextEquals(target, startIndex) =
      inner.SubTextEquals(target, startIndex)

    member _.ContentEquals sourceText = inner.ContentEquals sourceText

    member _.CopyTo(sourceIndex, destination, destinationIndex, count) =
      inner.CopyTo(sourceIndex, destination, destinationIndex, count)

    member _.GetSubTextFromRange range = inner.GetSubTextFromRange range

type MemberCategory =
  | ConstantField = 0
  | Field = 1
  | Constructor = 2
  | Event = 3
  | Property = 4
  | Abstract = 5 (* Instead of Indexer *)
  | Method = 6
  | NestedType = 7

type AccessLevel =
  | Public = 0
  | Internal = 1
  | Protected = 2
  | Private = 3

type MemberScope =
  | Static = 0
  | Instance = 1

type AccessModifierLevel =
  | Public
  | Private

and ScopeContext =
  { ModuleAccess: AccessModifierLevel
    TypeAccess: AccessModifierLevel option }

and CheckContext =
  { ModuleAccess: AccessModifierLevel }

type LintError =
  { Range: range
    Message: string
    LineContent: string
    ColumnIndicator: string }

and LintContext =
  { mutable Errors: LintError list
    Source: ISourceText
    FilePath: string
    EditorConfig: Configuration.EditorConfig }

and LintOutcome =
  { Index: int
    Path: string
    Ok: bool
    Log: string
    Errors: LintError list }

/// Everything the reading of one file keeps to itself, in one place.
///
/// The rules ask for this state on nearly every node they visit, and the ask
/// goes through `AsyncLocal`, which answers by walking the values the calling
/// context holds. Eleven of those are eleven entries to walk past on every
/// ask; one is one, and the runtime keeps the one-entry case in a shape that
/// needs no walk at all.
///
/// A write clones the state and installs the clone, so a write made while one
/// file is read is never seen while another is. The stores are the exception:
/// they are filled in place, and only after `beginReadings` has given the
/// reading of this file a state of its own.
type FileState =
  { mutable Path: string

    mutable EditorConfig: Configuration.EditorConfig

    mutable Context: LintContext option

    /// The lines the conditional-compilation directives of the file sit on.
    mutable DirectiveLines: int list

    mutable Trivia: ParsedInputTrivia

    /// The comments of the file, one bucket per line, holding what starts on
    /// it. Both comment searches ask what stands between two lines, and a run
    /// asks that of nearly every pair of neighbours in the tree; walking the
    /// whole list each time costs a run more than walking the tree does.
    mutable Comments: range list array

    /// Its conditional directives, bucketed the same way.
    mutable Directives: range list array

    /// The expressions standing as the argument of an application. A comma
    /// list there is a parameter list, which nothing else in the tree tells
    /// apart from a tuple of data, and the two are not laid out the same way:
    /// a parameter list too wide for its line breaks at every comma, while a
    /// tuple of data is asked to be named instead.
    mutable ApplicationArgs: HashSet<RangeKey>

    /// The expressions a match is taken on. A comma list there pairs the
    /// things being tested rather than building a value, so it is never asked
    /// for a name; what it answers for is only that its gaps agree.
    mutable MatchScrutinees: HashSet<RangeKey>

    /// Sub-chains already answered for by a longer chain above them. An
    /// operator chain nests to the left, so every prefix of it is an
    /// expression in its own right and would otherwise be judged again on its
    /// own; a short prefix of a long chain looks as though it could close up
    /// when the chain holding it cannot.
    mutable CoveredChains: HashSet<RangeKey>

    /// Applications already answered for by a longer one above them. A curried
    /// application nests to the left in the same way a chain does, so `f a b`
    /// is an expression of its own inside `f a b c` and would be judged twice.
    /// It is kept apart from the chains above so that noting one can never
    /// silence the other.
    mutable CoveredApplications: HashSet<RangeKey>

    /// Functions already answered for by one standing above them. A function
    /// written inside another is part of that one's body, so the two are not
    /// two lengths but one, and the outermost is where the demand belongs.
    mutable CoveredFunctions: ResizeArray<range>

    /// Where in `CoveredFunctions` the last question landed, or -1 for none.
    /// A tree is walked a branch at a time, so the function covering the node
    /// just asked about is nearly always the one covering this one, and asking
    /// it first turns a walk of the whole list into a single comparison.
    mutable LastCovered: int }
