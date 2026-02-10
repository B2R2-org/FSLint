[<AutoOpen>]
module B2R2.FSLint.Utils

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let [<Literal>] FakeFsPath = "FakeFsPathForUnitTest.fs"

let isPascalCase (methodName: string) =
  methodName.Length > 0 && Char.IsUpper(methodName[0])

let getAccessLevel = function
  | Some(SynAccess.Private _) -> Private
  | _ -> Public

let extractComparisonOperator = function
  | SynExpr.App(funcExpr = SynExpr.App(funcExpr = funcExpr)) ->
    match funcExpr with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ id ]))
    | SynExpr.Ident(ident = id) -> Some id.idText
    | _ -> None
  | _ -> None

/// Checks if there are compiler directives between two ranges
let findDirectivesBetween (trivia: ParsedInputTrivia) prev next =
  trivia.ConditionalDirectives
  |> List.tryFind (function
    | ConditionalDirectiveTrivia.If(_, range)
    | ConditionalDirectiveTrivia.Else range
    | ConditionalDirectiveTrivia.EndIf range ->
      range.StartLine > (prev: range).EndLine &&
      range.EndLine < (next: range).StartLine
  )

/// Checks if there are comments between two ranges using trivia information
let findCommentsBetween (trivia: ParsedInputTrivia) startRange endRange =
  trivia.CodeComments
  |> List.tryFind (function
    | CommentTrivia.LineComment range
    | CommentTrivia.BlockComment range ->
      range.StartLine >= (startRange: range).EndLine
      && range.EndLine <= (endRange: range).StartLine
      && Range.rangeContainsRange (Range.unionRanges startRange endRange) range)

/// Counts lines occupied by comments between two ranges
let countCommentLines (trivia: ParsedInputTrivia) (prev: range) next =
  trivia.CodeComments
  |> List.filter (function
    | CommentTrivia.LineComment r ->
      r.StartLine > prev.EndLine && r.EndLine < (next: range).StartLine
    | CommentTrivia.BlockComment r ->
      r.StartLine > prev.EndLine && r.EndLine < next.StartLine
  )
  |> List.sumBy (function
    | CommentTrivia.LineComment _ -> 1
    | CommentTrivia.BlockComment r -> r.EndLine - r.StartLine + 1)

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
  [ "*.fsproj"; "*.sln"; "*.slnx" ]
  |> Seq.collect (fun pattern ->
    Directory.EnumerateFiles(root, pattern, SearchOption.AllDirectories))
  |> Seq.sort
  |> Seq.toArray

let parseFile src (path: string) =
  let checker = FSharpChecker.Create()
  let projOptions, _ =
    checker.GetProjectOptionsFromScript(path, src)
    |> Async.RunSynchronously
  let parsingOptions, _ =
    checker.GetParsingOptionsFromProjectOptions projOptions
  checker.ParseFile(path, src, parsingOptions)
  |> Async.RunSynchronously
  |> fun r -> r.ParseTree