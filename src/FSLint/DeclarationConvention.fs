module B2R2.FSLint.DeclarationConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let collectFormattingRanges = function
  | SynModuleDecl.Open(_, range) -> "Open", range
  | SynModuleDecl.Types(_, range) -> "Types", range
  | SynModuleDecl.Let(_, _, range) -> "Let", range
  | SynModuleDecl.NestedModule(_, _, _, _, range, _) -> "Module", range
  | SynModuleDecl.Exception(_, range) -> "Exception", range
  | SynModuleDecl.Attributes(_, range) -> "Attributes", range
  | SynModuleDecl.HashDirective(_, range) -> "HashDirective", range
  | SynModuleDecl.ModuleAbbrev(_, _, range) -> "ModuleAbbrev", range
  | SynModuleDecl.Expr(_, range) -> "Expr", range
  | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(range = range)) ->
    "NamespaceFragment", range

let findLastBlockCommentEnd lines startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip (startIdx + 1)
  |> Array.tryFind (fun (_, line: string) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "*)" || line.TrimEnd().EndsWith "*)")
  |> Option.map fst

let countConsecutiveDocComments lines startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip startIdx
  |> Array.takeWhile (fun (_, line: string) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "///" || trimmed = "")
  |> Array.filter (fun (_, line) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "///")
  |> Array.length

let check (src: ISourceText) decls =
  decls
  |> List.map collectFormattingRanges
  |> List.pairwise
  |> List.iter (fun ((prevType, prevRange), (nextType, nextRange)) ->
    let expectedSpacing, actualSpacing =
      match prevType, nextType with
      | "Exception", "Exception"
      | "Attributes", "Expr"
      | "Open", "Open" -> 1, nextRange.StartLine - prevRange.EndLine
      | _ -> 2, nextRange.StartLine - prevRange.EndLine
    let adjustedSpacing =
      let subTextRange =
        Range.mkRange "" (Position.mkPos prevRange.EndLine 0)
          (Position.mkPos nextRange.StartLine 0)
      let subStr = src.GetSubTextFromRange subTextRange
      if subStr.Contains("#") then
        expectedSpacing
      else
        let lines =
          subStr.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
          |> Array.map (fun line -> line.Trim('\r'))
        if subStr.Trim() = "" || lines.Length <= 1 then
          actualSpacing
        else
          let findLineIndex pattern =
            lines |> Array.tryFindIndex (fun line -> pattern (line.TrimStart()))
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
            actualSpacing - countConsecutiveDocComments lines startIdx
          | _ -> actualSpacing
    if adjustedSpacing <> expectedSpacing then
      reportError src nextRange "Wrong newLine appear."
    else
      ()
  )