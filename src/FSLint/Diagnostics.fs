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

  /// Demands to close a group up, raised by a group reaching across a
  /// directive. Such a group holds different members in each build, so the
  /// demand is held here against the group that raised it until every build
  /// has been read.
  ///
  /// It is kept unless some build objects, not required of every build in
  /// turn: a build already closed up raises nothing and is no reason to
  /// leave the others open, while a build whose body can never come up is
  /// reason enough to ask no build to bring it. Each build names its own
  /// stray body, since that body is a different one in each.
  let deferredJoins = new AsyncLocal<ResizeArray<range * range>>()

  /// Groups some build cannot close up, whatever the others manage.
  let blockedGroups = new AsyncLocal<ResizeArray<range>>()

  /// The expressions standing as the argument of an application. A comma list
  /// there is a parameter list, which nothing else in the tree tells apart
  /// from a tuple of data, and the two are not laid out the same way: a
  /// parameter list too wide for its line breaks at every comma, while a
  /// tuple of data is asked to be named instead.
  let applicationArgs = new AsyncLocal<ResizeArray<range>>()

  let setCurrentFile (path: string) = currentFilePath.Value <- path

  let setCurrentLintContext (context: LintContext option) =
    currentLintContext.Value <- context

  let setDirectiveLines (lines: int list) = directiveLines.Value <- lines

  let beginReadings () =
    deferredJoins.Value <- ResizeArray()
    blockedGroups.Value <- ResizeArray()
    applicationArgs.Value <- ResizeArray()

  let noteApplicationArg (range: range) = applicationArgs.Value.Add range

  /// True when the stretch is an application's argument, and so a parameter
  /// list rather than a tuple standing on its own.
  let isApplicationArg (range: range) =
    match box applicationArgs.Value with
    | null ->
      false
    | _ ->
      applicationArgs.Value
      |> Seq.exists (fun (arg: range) ->
        arg.StartLine = range.StartLine && arg.StartColumn = range.StartColumn
        && arg.EndLine = range.EndLine && arg.EndColumn = range.EndColumn)

  let deferJoin (group: range) (body: range) =
    deferredJoins.Value.Add(group, body)

  let blockJoin (group: range) = blockedGroups.Value.Add group

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
        Range.mkRange ""
          (Position.mkPos 1 0)
          (Position.mkPos 1 0)
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
              fileName error.Range.StartLine error.Message)
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

  /// Raises the held demands no build objected to, and drops the rest.
  let reportAgreedJoins src =
    match box deferredJoins.Value with
    | null ->
      ()
    | _ ->
      let key (r: range) = r.StartLine, r.StartColumn, r.EndLine, r.EndColumn
      let blocked = blockedGroups.Value |> Seq.map key |> Set.ofSeq
      let isBlocked (group: range) = blocked |> Set.contains (key group)
      deferredJoins.Value
      |> Seq.filter (fun (group, _) -> not (isBlocked group))
      |> Seq.map snd
      |> Seq.distinctBy key
      |> Seq.sortBy key
      |> Seq.iter (reportNewLine src)
      deferredJoins.Value <- ResizeArray()
      blockedGroups.Value <- ResizeArray()

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
