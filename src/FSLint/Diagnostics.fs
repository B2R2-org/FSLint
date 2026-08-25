namespace B2R2.FSLint

open System
open System.IO
open System.Threading
open FSharp.Compiler.Text

exception LintException of string

module Diagnostics =
  let private outputLock = obj ()

  let currentFilePath = new AsyncLocal<string>()

  let cliEditorConfig = new AsyncLocal<Configuration.EditorConfig>()

  let currentLintContext = new AsyncLocal<LintContext option>()

  /// The lines the conditional-compilation directives of the file sit on.
  let directiveLines = new AsyncLocal<int list>()

  /// The expressions standing as the argument of an application. A comma list
  /// there is a parameter list, which nothing else in the tree tells apart
  /// from a tuple of data, and the two are not laid out the same way: a
  /// parameter list too wide for its line breaks at every comma, while a
  /// tuple of data is asked to be named instead.
  let applicationArgs = new AsyncLocal<ResizeArray<range>>()

  /// The expressions a match is taken on. A comma list there pairs the things
  /// being tested rather than building a value, so it is never asked for a
  /// name; what it answers for is only that its gaps agree.
  let matchScrutinees = new AsyncLocal<ResizeArray<range>>()

  /// Sub-chains already answered for by a longer chain above them. An operator
  /// chain nests to the left, so every prefix of it is an expression in its
  /// own right and would otherwise be judged again on its own; a short prefix
  /// of a long chain looks as though it could close up when the chain holding
  /// it cannot.
  let coveredChains = new AsyncLocal<ResizeArray<range>>()

  /// Applications already answered for by a longer one above them. A curried
  /// application nests to the left in the same way a chain does, so `f a b` is
  /// an expression of its own inside `f a b c` and would be judged twice. It is
  /// kept apart from the chains above so that noting one can never silence the
  /// other.
  let coveredApplications = new AsyncLocal<ResizeArray<range>>()

  /// Functions already answered for by one standing above them. A function
  /// written inside another is part of that one's body, so the two are not two
  /// lengths but one, and the outermost is where the demand belongs.
  let coveredFunctions = new AsyncLocal<ResizeArray<range>>()

  /// The name of the declaration now being walked into: a top-level binding,
  /// or the type where members are being read. A binding with no name of its
  /// own -- a `do` block -- is reported here, there being nowhere else to
  /// point that a reader could act on.
  let enclosingDecl = new AsyncLocal<range option>()

  let setCurrentFile (path: string) = currentFilePath.Value <- path

  let setCurrentLintContext (context: LintContext option) =
    currentLintContext.Value <- context

  let setDirectiveLines (lines: int list) = directiveLines.Value <- lines

  let beginReadings () =
    applicationArgs.Value <- ResizeArray()
    matchScrutinees.Value <- ResizeArray()
    coveredChains.Value <- ResizeArray()
    coveredFunctions.Value <- ResizeArray()
    coveredApplications.Value <- ResizeArray()
    enclosingDecl.Value <- None

  /// True when the store holds the very stretch given.
  let private holds (store: AsyncLocal<ResizeArray<range>>) (range: range) =
    match box store.Value with
    | null ->
      false
    | _ ->
      store.Value
      |> Seq.exists (fun (seen: range) ->
        seen.StartLine = range.StartLine && seen.StartColumn = range.StartColumn
        && seen.EndLine = range.EndLine && seen.EndColumn = range.EndColumn)

  let noteApplicationArg (range: range) = applicationArgs.Value.Add range

  let noteMatchScrutinee (range: range) = matchScrutinees.Value.Add range

  let noteCoveredChain (range: range) = coveredChains.Value.Add range

  let isCoveredChain range = holds coveredChains range

  let noteEnclosingDecl (range: range) = enclosingDecl.Value <- Some range

  /// Where the declaration now being walked is named, if it has a name.
  let enclosingDeclRange () = enclosingDecl.Value

  let noteCoveredFunction (range: range) = coveredFunctions.Value.Add range

  /// True when the stretch lies inside a function already noted, and so is
  /// part of a body that has answered for its length already.
  let isInsideCoveredFunction (range: range) =
    match box coveredFunctions.Value with
    | null ->
      false
    | _ ->
      coveredFunctions.Value
      |> Seq.exists (fun outer -> Range.rangeContainsRange outer range)

  let noteCoveredApplication (range: range) =
    coveredApplications.Value.Add range

  let isCoveredApplication range = holds coveredApplications range

  /// True when the stretch is an application's argument, and so a parameter
  /// list rather than a tuple standing on its own.
  let isApplicationArg range = holds applicationArgs range

  /// True when the stretch is what a match is taken on. Its commas pair the
  /// things being tested rather than build a value, so there is no tuple there
  /// to be given a name.
  let isMatchScrutinee range = holds matchScrutinees range

  /// True when a directive stands between the two lines, so that what they
  /// hold is not one stretch of code but a different stretch per build. A
  /// group reaching across such a line is read differently in each, and the
  /// two readings can want opposite things of it.
  let straddlesDirective (startLine: int) (endLine: int) =
    match box directiveLines.Value with
    | null ->
      false
    | _ ->
      directiveLines.Value
      |> List.exists (fun line -> line > startLine && line < endLine)

  let setCliEditorConfig config = cliEditorConfig.Value <- config

  let getCurrentMaxLineLength () =
    match currentLintContext.Value with
    | Some ctx -> ctx.EditorConfig.MaxLineLength
    | None -> cliEditorConfig.Value.MaxLineLength

  let exitWithError (message: string) =
    Console.WriteLine message
    exit 1

  let warn (message: string) = Console.Error.WriteLine message

  let raiseWithWarn (message: string) =
    match currentLintContext.Value with
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
    match currentLintContext.Value with
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
          let path = currentFilePath.Value
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
