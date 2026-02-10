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

  let setCurrentFile (path: string) = currentFilePath.Value <- path

  let setCurrentLintContext (context: LintContext option) =
    currentLintContext.Value <- context

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
    | None -> raise <| LintException message

  let reportWarn (src: ISourceText) (range: range) message =
    match currentLintContext.Value with
    | Some context ->
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
        if String.IsNullOrEmpty filePath then ""
        else Path.GetFileName filePath
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

  let reportInfixSpacing src range =
    reportWarn src range "Use whitespace around infix operator"

  let reportRangeOperatorError src range =
    reportWarn src range "Use whitespace around '..'"

  let reportBracketSpacingError src range =
    reportWarn src range "Use single whitespace between bracket and element"

  let reportBracketSpacingIncludeComment src range =
    reportWarn src range "Use single whitespace between bracket and comment"

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