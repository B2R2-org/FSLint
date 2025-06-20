[<AutoOpen>]
module B2R2.FSLint.Utils

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Compiler.Text

exception LintException of string

let raiseWithError (message: string) =
  raise <| LintException message

let exitWithError (message: string) =
  Console.WriteLine message
  exit 1

let warn (message: string) =
  Console.Error.WriteLine message

let reportError (src: ISourceText) (range: range) message =
  Console.Error.WriteLine (src.GetLineString (range.StartLine - 1))
  Console.Error.WriteLine (String.replicate range.StartColumn " " + "^")
  raiseWithError $"{range.StartLine} {message}"

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