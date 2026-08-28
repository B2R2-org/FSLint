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

open Diagnostics

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

/// Buckets the trivia by the line it starts on. A range starting past the last
/// line is dropped: it cannot stand between two lines the file does not reach.
let private bucketsOf (lineCount: int) (trivia: ParsedInputTrivia) =
  let comments = Array.create (lineCount + 2) []
  let directives = Array.create (lineCount + 2) []
  let add (buckets: range list array) (range: range) =
    if range.StartLine >= 0 && range.StartLine < buckets.Length then
      buckets[range.StartLine] <- range :: buckets[range.StartLine]
    else
      ()
  for comment in trivia.CodeComments do
    match comment with
    | CommentTrivia.LineComment range
    | CommentTrivia.BlockComment range -> add comments range
  for directive in trivia.ConditionalDirectives do
    match directive with
    | ConditionalDirectiveTrivia.If(_, range)
    | ConditionalDirectiveTrivia.Else range
    | ConditionalDirectiveTrivia.EndIf range -> add directives range
  comments, directives

/// Takes in the trivia of the file about to be read, bucketed by line.
let setTrivia (lineCount: int) (trivia: ParsedInputTrivia) =
  let comments, directives = bucketsOf lineCount trivia
  installTrivia trivia comments directives

/// Lets go of it once the file has been read.
let clearTrivia () = dropTrivia ()

/// The first range in the buckets from `firstLine` to `lastLine` that answers
/// `pick`. Reading only those buckets is what makes this cheap: the lines a
/// pair of neighbours spans are few, however many comments the file holds.
let private tryPickBetween (buckets: range list array) firstLine lastLine pick =
  let last = min lastLine (buckets.Length - 1)
  let mutable line = max 0 firstLine
  let mutable found = None
  while found.IsNone && line <= last do
    found <- buckets[line] |> List.tryFind pick
    line <- line + 1
  found

/// Checks if there are compiler directives between two ranges
let findDirectivesBetween prev next =
  match directiveBuckets () with
  | null ->
    None
  | buckets ->
    tryPickBetween buckets
                   (prev: range).EndLine
                   (next: range).StartLine
                   (fun range ->
                     range.StartLine > prev.EndLine
                     && range.EndLine < next.StartLine)

/// Everything in the buckets from `firstLine` to `lastLine` that answers
/// `pick`, taken as one stretch.
///
/// A gap can hold more than one comment, and a caller absorbing them into the
/// code on one side of the gap wants the far edge of the whole run: which one
/// came first is not what it is asking. Taking the union answers both
/// directions at once, and leaves the order the buckets happen to hold
/// without bearing on the answer.
let private unionBetween (buckets: range list array) firstLine lastLine pick =
  let last = min lastLine (buckets.Length - 1)
  let mutable found = None
  for line in max 0 firstLine .. last do
    for range in buckets[line] do
      if pick range then
        found <-
          match found with
          | Some sofar -> Some(Range.unionRanges sofar range)
          | None -> Some range
      else
        ()
  found

/// The comments between two ranges, as one stretch, using trivia information
let findCommentsBetween startRange endRange =
  match commentBuckets () with
  | null ->
    None
  | buckets ->
    let whole = Range.unionRanges startRange endRange
    unionBetween buckets
                 (startRange: range).EndLine
                 (endRange: range).StartLine
                 (fun range ->
                   range.StartLine >= startRange.EndLine
                   && range.EndLine <= endRange.StartLine
                   && Range.rangeContainsRange whole range)

let combineRangeWithComment startPos endPos combineToStartPos returnRange =
  match findCommentsBetween startPos endPos with
  | Some range ->
    if combineToStartPos then Range.unionRanges startPos range
    else Range.unionRanges range endPos
  | None ->
    returnRange

/// Counts lines occupied by comments between two ranges
let countCommentLines (prev: range) next =
  match commentBuckets () with
  | null ->
    0
  | buckets ->
    let last = min ((next: range).StartLine - 1) (buckets.Length - 1)
    let mutable total = 0
    for line in max 0 (prev.EndLine + 1) .. last do
      for range in buckets[line] do
        if range.EndLine < next.StartLine then
          total <- total + range.EndLine - range.StartLine + 1
        else
          ()
    total

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
/// The one checker every parse goes through. It is built to hold the caches a
/// parse reads, so one per file throws all of them away before they are used
/// twice.
let private checker = lazy FSharpChecker.Create()

/// The parsing options a run works with, once they are known. What the script
/// resolver answers does not turn on which file asked -- it is the conditional
/// defines and the language version -- but asking it costs a walk of the
/// script's references, and a run that asks per file spends most of its time
/// there.
let mutable private knownOptions: FSharpParsingOptions option = None

/// Asks the script resolver what the parsing options are.
let private resolveOptions (src: ISourceText) (path: string) =
  let checker = checker.Force()
  let projOptions, _ =
    checker.GetProjectOptionsFromScript(path, src)
    |> Async.RunSynchronously
  checker.GetParsingOptionsFromProjectOptions projOptions |> fst

/// Works the parsing options out ahead of a run, so that the parses of it read
/// them rather than each asking again. A run that never calls this still works:
/// every parse then resolves them as it goes, which is what one file wants.
let prepareParsing (src: ISourceText) (path: string) =
  knownOptions <- Some(resolveOptions src path)

let parseFile src (path: string) =
  let checker = checker.Force()
  let parsingOptions =
    match knownOptions with
    | Some options -> options
    | None -> resolveOptions src path
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