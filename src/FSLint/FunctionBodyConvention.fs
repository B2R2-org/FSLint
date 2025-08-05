module B2R2.FSLint.FunctionBodyConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

type SpacingRule =
  | NoSpacing = 1
  | SingleSpacing = 2

type FormattingInfo =
  { DeclType: string
    Range: Range
    SpacingBefore: SpacingRule
    GroupIndex: int
    ItemIndex: int }

/// Checks whether each binding within the given range in the source text
/// contains a newline within its scope. This helps enforce the convention
/// that function bodies should be multi-line if they exceed 80 columns.
let checkBindings (src: ISourceText) (range: range) bindings =
  let recurseIdxs =
    bindings
    |> List.skip 1
    |> List.map (fun (binding: SynBinding) ->
      binding.RangeOfBindingWithoutRhs.StartLine - 1
    )
  for recurseIdx in recurseIdxs do
    if src.GetLineString(recurseIdx - 1) <> "" then
      reportError src range "Nested should be separated by exactly one space."
    else ()
  for lineIdx in range.StartLine .. range.EndLine - 1 do
    if List.contains (lineIdx + 1) recurseIdxs then
      ()
    elif src.GetLineString lineIdx = "" then
      reportError src range "Remove empty line in function body."
    else ()

let rec check src = function
  | SynModuleDecl.NestedModule(decls = decls) ->
    for decl in decls do check src decl
  | SynModuleDecl.Let(_, bindings, range) ->
    checkBindings src range bindings
  | _ ->
    ()

let collectFormattingRanges = function
  | SynModuleDecl.Open(_, r) -> "Open", r
  | SynModuleDecl.Types(_, r) -> "Types", r
  | SynModuleDecl.Let(_, _, r) -> "Let", r
  | SynModuleDecl.NestedModule(_, _, _, _, r, _) -> "Module", r
  | SynModuleDecl.Exception(_, r) -> "Exception", r
  | SynModuleDecl.Attributes(_, r) -> "Attributes", r
  | SynModuleDecl.HashDirective(_, r) -> "HashDirective", r
  | SynModuleDecl.ModuleAbbrev(_, _, r) -> "ModuleAbbrev", r
  | SynModuleDecl.Expr(_, r) -> "Expr", r
  | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(range = range)) ->
    "NamespaceFragment", range

let countConsecutiveComments (lines: string[]) startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip startIdx
  |> Array.takeWhile (fun (_, line) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "///" || trimmed = "")
  |> Array.filter (fun (_, line) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "///")
  |> Array.length

let findLastBlockCommentEnd (lines: string[]) startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip (startIdx + 1)
  |> Array.tryFind (fun (_, line) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "*)" || line.TrimEnd().EndsWith "*)")
  |> Option.map fst

let checkTop (src: ISourceText) decls =
  decls
  |> List.map collectFormattingRanges
  |> List.pairwise
  |> List.iter (fun ((prevType, prevRange), (nextType, nextRange)) ->
    let expectedSpacing, actualSpacing =
      let diff = nextRange.StartLine - prevRange.EndLine
      match prevType, nextType with
      | "Exception", "Exception"
      | "Attributes", "Expr"
      | "Open", "Open" -> SpacingRule.NoSpacing, diff
      | _ -> SpacingRule.SingleSpacing, diff
    let adjustedSpacing =
      let subTextRange =
        Range.mkRange "" (Position.mkPos prevRange.EndLine 0)
          (Position.mkPos nextRange.StartLine 0)
      let subStr = src.GetSubTextFromRange subTextRange
      if subStr.Contains("#") then (* Compiler flag cannot check *)
        expectedSpacing.GetHashCode()
      else
        let lines =
          subStr.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
          |> Array.map (fun line -> line.Trim('\r'))
        if subStr.Trim() = "" || lines.Length <= 1 then
          actualSpacing
        else
          let findLineIndex pattern =
            lines
            |> Array.tryFindIndex (fun line ->
              let trimmed = line.TrimStart()
              pattern trimmed)
          let blockCommentStart =
            findLineIndex (fun line -> line.StartsWith "(*")
          let docCommentStart =
            findLineIndex (fun line -> line.StartsWith "///")
          match blockCommentStart, docCommentStart with
          | Some startIdx, _ ->
            match findLastBlockCommentEnd lines startIdx with
            | Some endIdx ->
              actualSpacing - (endIdx - startIdx + 1)
            | None ->
              actualSpacing - (lines.Length - startIdx - 1)
          | _, Some startIdx ->
            actualSpacing - countConsecutiveComments lines startIdx
          | _ -> actualSpacing
    if adjustedSpacing <> expectedSpacing.GetHashCode() then
      reportError src nextRange "Wrong newLine appear."
    else ()
  )