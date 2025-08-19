module B2R2.FSLint.DeclarationConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let tryFindBlockCommentEndAfter lines startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip (startIdx + 1)
  |> Array.tryFind (fun (_, line: string) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "*)" || line.TrimEnd().EndsWith "*)")
  |> Option.map fst

let countLeadingDocComments lines startIdx =
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

let calculateSpacingBetweenDecls (src: ISourceText) prevDecl nextDecl =
  let normalCase =
    match prevDecl, nextDecl with
    | SynModuleDecl.Attributes _, _
    | SynModuleDecl.Open _, SynModuleDecl.Open _ -> 1
    | _ -> 2
  let lastLineStr = src.GetLineString(prevDecl.Range.EndLine - 1)
  if lastLineStr.TrimStart().StartsWith "[<"
    && lastLineStr.TrimStart().EndsWith ">]"
    && normalCase = 2
  then normalCase - 1
  else normalCase

/// Determines line breaks between declarations based on their types.
/// Skips validation if compiler directives are present.
/// Adjusts range when handling block comments (* *) or doc comments (///).
let adjustByComment src prevRange nextRange expect actual =
  let createRangeBetweenDecl =
    Range.mkRange "" (Position.mkPos (prevRange: range).EndLine 0)
      (Position.mkPos (nextRange: range).StartLine 0)
  let subStr = (src: ISourceText).GetSubTextFromRange createRangeBetweenDecl
  if subStr.Contains("#") then
    expect
  else
    let lines =
      subStr.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
      |> Array.map (fun line -> line.Trim('\r'))
    if subStr.Trim() = "" || lines.Length <= 1 then
      actual
    else
      let findLineIndex pattern =
        lines |> Array.tryFindIndex (fun line -> pattern (line.TrimStart()))
      let blockCommentStart =
        findLineIndex (fun line -> line.StartsWith "(*")
      let docCommentStart =
        findLineIndex (fun line -> line.StartsWith "///")
      match blockCommentStart, docCommentStart with
      | Some startIdx, _ ->
        match tryFindBlockCommentEndAfter lines startIdx with
        | Some endIdx ->
          actual - (endIdx - startIdx + 1)
        | None ->
          actual - (lines.Length - startIdx - 1)
      | _, Some startIdx ->
        actual - countLeadingDocComments lines startIdx
      | _ -> actual

let checkEqualSpacing (src: ISourceText) (range: range option) =
  if range.IsSome then
    if src.GetLineString(range.Value.EndLine - 1).EndsWith "=" then
      (Position.mkPos range.Value.EndLine (range.Value.StartColumn - 1),
       range.Value.Start)
      ||> Range.mkRange ""
      |> src.GetSubTextFromRange
      |> fun subStr ->
        if subStr <> " " then
          reportError src range.Value "Need space before '='."
        else
          ()
    else
      (Position.mkPos range.Value.EndLine (range.Value.StartColumn - 1),
       Position.mkPos range.Value.EndLine (range.Value.EndColumn + 1))
       ||> Range.mkRange ""
       |> src.GetSubTextFromRange
       |> fun subStr ->
         if subStr <> " = " then
           reportError src range.Value "Need space before and after '='."
         else
           ()
  else
    ()

let check (src: ISourceText) decls =
  decls
  |> List.pairwise
  |> List.iter (fun ((prevDecl: SynModuleDecl), nextDecl) ->
    if prevDecl.IsLet && nextDecl.IsLet then
      let expectedSpacing = calculateSpacingBetweenDecls src prevDecl nextDecl
      let actualSpacing =
        nextDecl.Range.StartLine - prevDecl.Range.EndLine
        |> adjustByComment src prevDecl.Range nextDecl.Range expectedSpacing
      if actualSpacing <> expectedSpacing then
        reportError src nextDecl.Range "Wrong newLine appear."
      else
        ()
    else
      ()
  )