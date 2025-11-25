[<AutoOpen>]
module B2R2.FSLint.Utils

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open FSharp.Compiler.Text

exception LintException of string

type LintError =
  { Range: range
    Message: string
    LineContent: string
    ColumnIndicator: string }

type LintContext =
  { mutable Errors: LintError list
    Source: ISourceText
    FilePath: string }

let private outputLock = obj ()

let private currentFilePath = new Threading.AsyncLocal<string>()

let private currentLintContext = new Threading.AsyncLocal<LintContext option>()

let setCurrentFile (path: string) = currentFilePath.Value <- path

let setCurrentLintContext (context: LintContext option) =
  currentLintContext.Value <- context

let getCurrentLintContext () = currentLintContext.Value

let raiseWithError (message: string) =
  match getCurrentLintContext () with
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

let exitWithError (message: string) =
  Console.WriteLine message
  exit 1

let warn (message: string) = Console.Error.WriteLine message

[<Literal>]
let MaxLineLength = 80

let reportError (src: ISourceText) (range: range) message =
  match getCurrentLintContext () with
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
        if isNull path || String.IsNullOrEmpty(path) then ""
        else Path.GetFileName(path)
      if String.IsNullOrEmpty(fileName) then
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
    raiseWithError $"{range.StartLine} {message}"

let reportErrors (errors: LintError list) (filePath: string) =
  lock outputLock (fun () ->
    let fileName =
      if String.IsNullOrEmpty(filePath) then ""
      else Path.GetFileName(filePath)
    for error in List.rev errors do
      if String.IsNullOrEmpty(fileName) then
        Console.Error.WriteLine(
          sprintf "Line %d: %O" error.Range.StartLine error.Message)
      else
        Console.Error.WriteLine(
          sprintf "[%s] Line %d: %O"
            fileName error.Range.StartLine error.Message)
      Console.Error.WriteLine(error.LineContent)
      Console.Error.WriteLine(error.ColumnIndicator)
  )

let runOnEveryFsFile (path: string) (action: string -> unit) =
  let sep = Path.DirectorySeparatorChar |> string |> Regex.Escape
  let exclusionPatterns =
    [| Regex $"obj{sep}Debug{sep}"
       Regex $"obj{sep}Release{sep}"
       Regex $"CFG.Tests" |]
  let searchOpt = SearchOption.AllDirectories
  for f in Directory.EnumerateFiles(path, "*.fs", searchOpt) do
    if exclusionPatterns |> Array.exists (fun r -> r.IsMatch f) then ()
    else action f

let runOnEveryProjectSlnFile (path: string) (action: string -> unit) =
  let searchOpt = SearchOption.AllDirectories
  for f in Directory.EnumerateFiles(path, "*.fsproj", searchOpt) do action f
  for f in Directory.EnumerateFiles(path, "*.sln", searchOpt) do action f