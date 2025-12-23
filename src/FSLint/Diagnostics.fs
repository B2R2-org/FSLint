namespace B2R2.FSLint

open System
open System.IO
open FSharp.Compiler.Text

exception LintException of string

module Diagnostics =
  let private outputLock = obj ()

  let private currentFilePath = new Threading.AsyncLocal<string>()

  let currentLintContext = new Threading.AsyncLocal<LintContext option>()

  let setCurrentFile (path: string) = currentFilePath.Value <- path

  let setCurrentLintContext (context: LintContext option) =
    currentLintContext.Value <- context

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

  let reportRedundant src range scopeType =
    reportWarn src range
      $"Redundant 'private' modifier: restricted by enclosing {scopeType}"

  let reportInfixError src range =
    reportWarn src range "There must be a space before and after the infix"

  let reportRangeOperatorError src range =
    reportWarn src range "Spaces required around range operator"

  let reportBracketSpacingError src range =
    reportWarn src range "Wrong spacing inside brackets"

  let reportSingleElementPerLineError src range =
    reportWarn src range "Only one element per line allowed"

  let reportPascalCaseError src range =
    reportWarn src range "No space between ident and paren"

  let reportLowerCaseError src range =
    reportWarn src range "Need single space between ident and paren"

  let reportBarAndPatternError src range =
    reportWarn src range "Need single space between bar and pattern."

  let reportBarAndMatchError src range =
    reportWarn src range "Bar '|' not aligned with 'match' keyword."

  let reportArrowError src range =
    reportWarn src range "Need a single space around arrow."

  let reportTypeError src range =
    reportWarn src range "Type annotation does not follow the convention."
