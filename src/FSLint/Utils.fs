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
  Console.Error.WriteLine(src.GetLineString(range.StartLine - 1))
  Console.Error.WriteLine(String.replicate range.StartColumn " " + "^")
  raiseWithError $"{range.StartLine} {message}"

let parseResult okay = function
  | Success -> okay
  | Failure(errMsg) -> printfn "%s" errMsg; false

let runOnEveryFsFile path action =
  let searchOpt = SearchOption.AllDirectories
  let sep = Path.DirectorySeparatorChar |> string |> Regex.Escape
  let exclusionPatterns =
    [| Regex $"obj{sep}Debug{sep}"
       Regex $"obj{sep}Release{sep}"
       Regex $"CFG.Tests" |]
  Directory.EnumerateFiles(path, "*.fs", searchOpt)
  |> Seq.filter (fun path ->
    exclusionPatterns
    |> Array.exists (fun r -> r.IsMatch(path))
    |> not)
  |> Seq.map action
  |> Seq.fold parseResult true

let runOnEveryProjectSlnFile path action =
  let searchOpt = SearchOption.AllDirectories
  Directory.EnumerateFiles(path, "*.fsproj", searchOpt)
  |> Seq.append <| Directory.EnumerateFiles(path, "*.sln", searchOpt)
  |> Seq.map action
  |> Seq.fold parseResult true