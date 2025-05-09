[<AutoOpen>]
module B2R2.FSLint.Utils

open System
open System.IO
open System.Text.RegularExpressions

exception LintException of string

let raiseWithError (message: string) =
  raise <| LintException message

let exitWithError (message: string) =
  Console.WriteLine message
  exit 1

let runOnEveryFile (path: string) (action: string -> unit) =
  let sep = Path.DirectorySeparatorChar |> string |> Regex.Escape
  let exclusionPatterns =
    [| Regex $"obj{sep}Debug{sep}"
       Regex $"obj{sep}Release{sep}"
       Regex $"CFG.Tests" |]
  let searchOpt = SearchOption.AllDirectories
  for f in Directory.EnumerateFiles(path, "*.fs", searchOpt) do
    if exclusionPatterns |> Array.exists (fun r -> r.IsMatch f) then ()
    else action f