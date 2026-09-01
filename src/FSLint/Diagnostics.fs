namespace B2R2.FSLint

open System
open System.Collections.Generic
open System.IO
open System.Threading
open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTrivia

exception LintException of string

module Diagnostics =
  let private outputLock = obj ()

  /// The state of the file being read. One `AsyncLocal` and not one per thing
  /// kept, for the reason `FileState` gives.
  let private perFile = AsyncLocal<FileState>()

  let private emptyState () =
    { Path = null
      EditorConfig = Configuration.defaultSettings
      Context = None
      DirectiveLines = []
      Trivia = Unchecked.defaultof<ParsedInputTrivia>
      Comments = null
      Directives = null
      ApplicationArgs = null
      MatchScrutinees = null
      CoveredChains = null
      CoveredApplications = null
      CoveredFunctions = null
      LastCovered = -1 }

  /// What a task holding no state of its own reads. Nothing writes here: a
  /// write installs a clone, so this stays as empty as it was made.
  let private blank = emptyState ()

  /// The state of the file this task is reading.
  let internal current () =
    let state = perFile.Value
    if obj.ReferenceEquals(state, null) then blank else state

  /// Hands this task a state of its own, leaving the state another task reads
  /// its own file against untouched.
  let private install (state: FileState) = perFile.Value <- state

  let setCurrentFile (path: string) =
    install { current () with Path = path }

  let setCurrentLintContext (context: LintContext option) =
    install { current () with Context = context }

  let currentLintContext () = (current ()).Context

  let setDirectiveLines (lines: int list) =
    install { current () with DirectiveLines = lines }

  /// Puts the trivia of the file, and the buckets read from it, in reach of
  /// the searches over them.
  let internal installTrivia trivia comments directives =
    install { current () with
                Trivia = trivia
                Comments = comments
                Directives = directives }

  /// Puts them out of reach again once the file has been read.
  let internal dropTrivia () =
    install { current () with
                Trivia = Unchecked.defaultof<ParsedInputTrivia>
                Comments = null
                Directives = null }

  let internal currentTrivia () = (current ()).Trivia

  let internal commentBuckets () = (current ()).Comments

  let internal directiveBuckets () = (current ()).Directives

  let beginReadings () =
    install { current () with
                ApplicationArgs = HashSet()
                MatchScrutinees = HashSet()
                CoveredChains = HashSet()
                CoveredApplications = HashSet()
                CoveredFunctions = ResizeArray()
                LastCovered = -1 }

  /// What identifies a stretch, so that a store can be keyed by it.
  let inline private keyOf (range: range) =
    RangeKey(range.StartLine, range.StartColumn, range.EndLine, range.EndColumn)

  /// True when the store holds the very stretch given.
  let private holds (store: HashSet<RangeKey>) (range: range) =
    not (isNull store) && store.Contains(keyOf range)

  let noteApplicationArg (range: range) =
    (current ()).ApplicationArgs.Add(keyOf range) |> ignore

  let noteMatchScrutinee (range: range) =
    (current ()).MatchScrutinees.Add(keyOf range) |> ignore

  let noteCoveredChain (range: range) =
    (current ()).CoveredChains.Add(keyOf range) |> ignore

  let isCoveredChain range = holds (current ()).CoveredChains range

  let noteCoveredFunction (range: range) =
    (current ()).CoveredFunctions.Add range

  /// True when the stretch lies inside a function already noted, and so is
  /// part of a body that has answered for its length already. The one that
  /// answered last is asked first: a walk goes down a branch before it goes
  /// along, so the answer is nearly always the same as the one before it.
  let isInsideCoveredFunction (range: range) =
    let state = current ()
    match state.CoveredFunctions with
    | null ->
      false
    | outers ->
      let covers index = Range.rangeContainsRange outers[index] range
      if state.LastCovered >= 0 && covers state.LastCovered then
        true
      else
        let mutable index = 0
        let mutable found = false
        while not found && index < outers.Count do
          if covers index then
            state.LastCovered <- index
            found <- true
          else
            index <- index + 1
        found

  let noteCoveredApplication (range: range) =
    (current ()).CoveredApplications.Add(keyOf range) |> ignore

  let isCoveredApplication range = holds (current ()).CoveredApplications range

  /// True when the stretch is an application's argument, and so a parameter
  /// list rather than a tuple standing on its own.
  let isApplicationArg range = holds (current ()).ApplicationArgs range

  /// True when the stretch is what a match is taken on. Its commas pair the
  /// things being tested rather than build a value, so there is no tuple there
  /// to be given a name.
  let isMatchScrutinee range = holds (current ()).MatchScrutinees range

  /// True when a directive stands between the two lines, so that what they
  /// hold is not one stretch of code but a different stretch per build. A
  /// group reaching across such a line is read differently in each, and the
  /// two readings can want opposite things of it.
  let straddlesDirective (startLine: int) (endLine: int) =
    (current ()).DirectiveLines
    |> List.exists (fun line -> line > startLine && line < endLine)

  let setCliEditorConfig config =
    install { current () with EditorConfig = config }

  let getCurrentMaxLineLength () =
    let state = current ()
    match state.Context with
    | Some ctx -> ctx.EditorConfig.MaxLineLength
    | None -> state.EditorConfig.MaxLineLength

  let exitWithError (message: string) =
    Console.WriteLine message
    exit 1

  let warn (message: string) = Console.Error.WriteLine message

  let raiseWithWarn (message: string) =
    match currentLintContext () with
    | Some context ->
      let dummyRange =
        Range.mkRange "" (Position.mkPos 1 0) (Position.mkPos 1 0)
      let error =
        { Range = dummyRange
          Message = message
          LineContent = ""
          ColumnIndicator = "" }
      context.Errors <- error :: context.Errors
    | None ->
      raise <| LintException message

  let reportWarn (src: ISourceText) (range: range) message =
    match currentLintContext () with
    | Some context ->
      let isDuplicate =
        context.Errors
        |> List.exists (fun e ->
          e.Message = message
          && e.Range.Start = range.Start
          && e.Range.End = range.End)
      if isDuplicate then
        ()
      else
        let lineContent = src.GetLineString(range.StartLine - 1)
        let columnIndicator = String.replicate range.StartColumn " " + "^"
        let error =
          { Range = range
            Message = message
            LineContent = lineContent
            ColumnIndicator = columnIndicator }
        context.Errors <- error :: context.Errors
    | None ->
      lock outputLock (fun () ->
        let fileName =
          let path = (current ()).Path
          if isNull path || String.IsNullOrEmpty path then ""
          else Path.GetFileName path
        if String.IsNullOrEmpty fileName then
          Console.Error.WriteLine(
            sprintf "Line %d: %O" range.StartLine message)
        else
          Console.Error.WriteLine(
            sprintf "[%s] Line %d: %O" fileName range.StartLine message)
        Console.Error.WriteLine(
          src.GetLineString(range.StartLine - 1))
        Console.Error.WriteLine(
          String.replicate range.StartColumn " " + "^")
      )
      raiseWithWarn $"{range.StartLine} {message}"

  let reportWarns (errors: LintError list) (filePath: string) =
    lock outputLock (fun () ->
      let fileName =
        if String.IsNullOrEmpty filePath then "" else Path.GetFileName filePath
      for error in List.rev errors do
        if String.IsNullOrEmpty fileName then
          Console.Error.WriteLine(
            sprintf "Line %d: %O" error.Range.StartLine error.Message)
        else
          Console.Error.WriteLine(
            sprintf "[%s] Line %d: %O"
              fileName
              error.Range.StartLine
              error.Message)
        Console.Error.WriteLine error.LineContent
        Console.Error.WriteLine error.ColumnIndicator
    )

[<AutoOpen>]
module CustomReports =
  open Diagnostics

  let reportRedundant src range =
    reportWarn src range $"Remove Redundant 'private'"

  let reportNewLine src range =
    reportWarn src range "Remove unnecessary line break"

  let reportBindToLet src range = reportWarn src range "Bind to fit the line"

  /// A body opening a line of its own at the wrong column. What it is told is
  /// where the body belongs rather than how far out it went: the number it is
  /// out by is of no use to anyone reading the line.
  let reportBodyIndent src range =
    reportWarn src range "Indent the body by two columns"

  /// A function body run past the budget. What it is told is what to do about
  /// it rather than how long it is: the length is a symptom, and the count is
  /// already in the settings for anyone who wants it.
  let reportLongFunction src range =
    reportWarn src range "Split into smaller functions"

  /// A `when` clause still sharing the line its type parameters stand on.
  /// Sending it down takes the constraints below it along, so it is the only
  /// thing said of such a list.
  let reportWhenPlacement src range =
    reportWarn src range "Move 'when' to the next line"

  /// An `and` that does not stand in the column its `when` opened.
  let reportAndAlignment src range =
    reportWarn src range "Align 'and' with 'when'"

/// We intentionally do not suggest a concrete fix here because some malformed
/// operator-spacing cases (for example, generic-looking syntax parsed as infix
/// operators) can require either adding or removing whitespace depending on how
/// the code was parsed.
  let reportInfixSpacing src range =
    reportWarn src range "Incorrect operator spacing"

  let reportRangeOperatorError src range =
    reportWarn src range "Use whitespace around '..'"

  let reportBracketSpacingError src range =
    reportWarn src range "Use single whitespace between bracket and element"

  /// A pair of brackets placed differently: one beside what it fences and the
  /// other on a line of its own. A record definition asks this of its braces
  /// as a literal asks it of its brackets, and the report points at the
  /// closing one, since the opening one is what sets the layout.
  let reportBracketSymmetry src range =
    reportWarn src range "Use consistent bracket placement"

  let reportBracketIndentation src range =
    reportWarn src range "Use 2 indentation"

  let reportBracketNoSpacingError src range =
    reportWarn src range "Remove whitespace in brackets"

  let reportSingleElementPerLineError src range =
    reportWarn src range "Use one element per line"

  let reportPascalCaseError src range =
    reportWarn src range "Remove whitespace before '('"

  let reportLowerCaseError src range =
    reportWarn src range "Use single whitespace before '('"

  let reportFrontParenInnerSpacing src range =
    reportWarn src range "Remove whitespace after '('"

  let reportBackParenInnerSpacing src range =
    reportWarn src range "Remove whitespace before ')'"

  let reportBarBeforeSpacing src range =
    reportWarn src range "Use single whitespace before '|'"

  let reportBarAfterSpacing src range =
    reportWarn src range "Use single whitespace after '|'"

  let reportBarAndMatchError src range =
    reportWarn src range "Align '|' with 'match'"

  let reportTrailingSeparator src range =
    reportWarn src range "Remove trailing ';'"

  let reportArrowBeforeSpacing src range =
    reportWarn src range "Use single whitespace before '->'"

  let reportArrowAfterSpacing src range =
    reportWarn src range "Use single whitespace after '->'"

  let reportEqaulBeforeSpacing src range =
    reportWarn src range "Use single whitespace before '='"

  let reportEqaulAfterSpacing src range =
    reportWarn src range "Use single whitespace after '='"

  let reportSemiColonBeforeSpacing src range =
    reportWarn src range "Remove whitespace before ';'"

  let reportSemiColonAfterSpacing src range =
    reportWarn src range "Use single whitespace after ';'"

  let reportLeftAngleSpacing src range =
    reportWarn src range "Remove whitespace before '<'"

  let reportLeftAngleInnerSpacing src range =
    reportWarn src range "Remove whitespace after '<'"

  let reportRightAngleInnerSpacing src range =
    reportWarn src range "Remove whitespace before '>'"

  let reportLeftCurlyBraceSpacing src range =
    reportWarn src range "Use single whitespace after '{'"

  let reportRightCurlyBraceSpacing src range =
    reportWarn src range "Use single whitespace before '}'"

  let reportCommaFormat src range = reportWarn src range "Use ', '"

  let reportStarFormat src range = reportWarn src range "Use ' * '"

  let reportCommaBeforeSpacing src range =
    reportWarn src range "Remove whitespace before ','"

  let reportCommaAfterSpacing src range =
    reportWarn src range "Use single whitespace after ','"

  let reportMemberCurried src range =
    reportWarn src range "Use non-curried parameter style"

  let reportConsecutiveSpacing src range =
    reportWarn src range "Remove consecutive whitespace"
