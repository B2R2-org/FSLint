[<AutoOpen>]
module B2R2.FSLint.Utils

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let [<Literal>] MaxLineLength = 80

let isPascalCase (methodName: string) =
  methodName.Length > 0 && Char.IsUpper(methodName[0])

let getAccessLevel = function
  | Some(SynAccess.Private _) -> Private
  | _ -> Public

/// Collects all .fs source files under the given root directory
let getFsFiles (root: string) =
  let sep = Path.DirectorySeparatorChar |> string |> Regex.Escape
  let exclusion =
    [| Regex $"obj{sep}Debug{sep}"
       Regex $"obj{sep}Release{sep}"
       Regex $"CFG.Tests" |]
  Directory.EnumerateFiles(root, "*.fs", SearchOption.AllDirectories)
  |> Seq.filter
    (fun f -> not (exclusion |> Array.exists (fun r -> r.IsMatch f)))
  |> Seq.sort
  |> Seq.toArray

/// Collects .fsproj and .sln project/solution files
let getProjOrSlnFiles (root: string) =
  [ "*.fsproj"; "*.sln" ]
  |> Seq.collect (fun pattern ->
    Directory.EnumerateFiles(root, pattern, SearchOption.AllDirectories))
  |> Seq.sort
  |> Seq.toArray

let parseFile txt (path: string) =
  let checker = FSharpChecker.Create()
  let src = SourceText.ofString txt
  let projOptions, _ =
    checker.GetProjectOptionsFromScript(path, src)
    |> Async.RunSynchronously
  let parsingOptions, _ =
    checker.GetParsingOptionsFromProjectOptions projOptions
  checker.ParseFile(path, src, parsingOptions)
  |> Async.RunSynchronously
  |> fun r -> src, r.ParseTree
