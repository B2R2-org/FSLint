[<AutoOpen>]
module B2R2.FSLint.Utils

open System
open System.IO
open System.Threading
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let asyncLocal = AsyncLocal<ParsedInputTrivia>()

let mutable isStrict = false

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
  | _ ->
    None

let isBlankLine (src: ISourceText) lineIdx =
  src.GetLineString(lineIdx - 1) |> String.IsNullOrWhiteSpace

/// Checks if there are compiler directives between two ranges
let findDirectivesBetween prev next =
  asyncLocal.Value.ConditionalDirectives
  |> List.tryFind (function
    | ConditionalDirectiveTrivia.If(_, range)
    | ConditionalDirectiveTrivia.Else range
    | ConditionalDirectiveTrivia.EndIf range ->
      range.StartLine > (prev: range).EndLine &&
      range.EndLine < (next: range).StartLine
  )

/// Checks if there are comments between two ranges using trivia information
let findCommentsBetween startRange endRange =
  asyncLocal.Value.CodeComments
  |> List.tryPick (function
    | CommentTrivia.LineComment range
    | CommentTrivia.BlockComment range when
      range.StartLine >= (startRange: range).EndLine
      && range.EndLine <= (endRange: range).StartLine
      && Range.rangeContainsRange (Range.unionRanges startRange endRange) range
      -> Some range
    | _ -> None)

let combineRangeWithComment startPos endPos combineToStartPos returnRange =
  match findCommentsBetween startPos endPos with
  | Some range ->
    if combineToStartPos then Range.unionRanges startPos range
    else Range.unionRanges range endPos
  | None ->
    returnRange

/// Counts lines occupied by comments between two ranges
let countCommentLines (prev: range) next =
  asyncLocal.Value.CodeComments
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

/// The symbols named by the `#if` and `#elif` directives of a file.
let private conditionalSymbols (src: ISourceText) =
  let namesOf (line: string) =
    Regex.Matches(line, "[A-Za-z_][A-Za-z0-9_]*")
    |> Seq.map (fun m -> m.Value)
    |> Seq.filter (fun name -> name <> "if" && name <> "elif")
    |> Seq.toList
  [ 0 .. src.GetLineCount() - 1 ]
  |> List.map (fun i -> src.GetLineString(i).TrimStart())
  |> List.filter (fun line ->
    line.StartsWith "#if " || line.StartsWith "#elif ")
  |> List.collect namesOf
  |> List.distinct

/// The lines the conditional-compilation directives of the file sit on.
let directiveLinesOf (src: ISourceText) =
  [ 0 .. src.GetLineCount() - 1 ]
  |> List.map (fun i -> i, src.GetLineString(i).TrimStart())
  |> List.filter (fun (_, line) ->
    line.StartsWith "#if" || line.StartsWith "#else"
    || line.StartsWith "#elif" || line.StartsWith "#endif")
  |> List.map (fun (i, _) -> i + 1)

/// Parses the file as the compiler would see it, and again with every symbol
/// its `#if` directives name defined. A branch left out of the first parse is
/// absent from that tree altogether and no rule can reach it, so the second
/// parse brings it in; between the two, both sides of a plain `#if`/`#else`
/// are read. A file naming no symbols is parsed once.
let parseFile src (path: string) =
  let checker = FSharpChecker.Create()
  let projOptions, _ =
    checker.GetProjectOptionsFromScript(path, src)
    |> Async.RunSynchronously
  let parsingOptions, _ =
    checker.GetParsingOptionsFromProjectOptions projOptions
  let parseWith (options: FSharpParsingOptions) =
    checker.ParseFile(path, src, options)
    |> Async.RunSynchronously
    |> fun r -> r.ParseTree
  match (if isStrict then conditionalSymbols src else []) with
  | [] ->
    [ parseWith parsingOptions ]
  | symbols ->
    let withSymbols =
      { parsingOptions with
          ConditionalDefines = symbols @ parsingOptions.ConditionalDefines }
    [ parseWith parsingOptions; parseWith withSymbols ]