module B2R2.FSLint.DeclarationConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let private tryFindBlockCommentEndAfter lines startIdx =
  lines
  |> Array.mapi (fun i line -> i, line)
  |> Array.skip (startIdx + 1)
  |> Array.tryFind (fun (_, line: string) ->
    let trimmed = line.TrimStart()
    trimmed.StartsWith "*)" || line.TrimEnd().EndsWith "*)")
  |> Option.map fst

let private countLeadingDocComments lines startIdx =
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

let private calculateSpacingBetweenDecls (src: ISourceText) prevDecl nextDecl =
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
let private adjustByComment src prevRange nextRange expect actual =
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
      let blockCommentStart =
        lines
        |> Array.tryFindIndex (fun line ->
          (line.TrimStart()).StartsWith "(*")
      let docCommentStart =
        lines
        |> Array.tryFindIndex (fun line ->
          (line.TrimStart()).StartsWith "///")
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
          reportWarn src range.Value "Need space before '='."
        else
          ()
    else
      (Position.mkPos range.Value.EndLine (range.Value.StartColumn - 1),
       Position.mkPos range.Value.EndLine (range.Value.EndColumn + 1))
       ||> Range.mkRange ""
       |> src.GetSubTextFromRange
       |> fun subStr ->
         if subStr <> " = " then
           reportWarn src range.Value "Need space before and after '='."
         else
           ()
  else
    ()

let checkLetAndMultilineRhsPlacement (src: ISourceText) (binding: SynBinding) =
  let SynBinding(expr = body; trivia = trivia) = binding
  match trivia.EqualsRange with
  | Some eqRange ->
    match body with
    | SynExpr.Const(SynConst.String(synStringKind = stringKind), _)
      when stringKind = SynStringKind.TripleQuote
      && eqRange.StartLine = body.Range.StartLine
      && (body.Range.StartLine <> body.Range.EndLine) ->
        reportWarn src body.Range "Triple-quoted should be on the next line."
    | _ ->
      ()
  | None ->
    ()

let checkUnnecessaryLineBreak (src: ISourceText) (binding: SynBinding) =
  let SynBinding(headPat = pattern; expr = body; range = bindingRange;
                 trivia = trivia) = binding
  if body.Range.StartLine = body.Range.EndLine then
    match trivia.EqualsRange with
    | Some eqRange ->
      if eqRange.EndLine < body.Range.StartLine then
        let fullRange =
          Range.mkRange ""
            (Position.mkPos pattern.Range.StartLine 0)
            (Position.mkPos body.Range.EndLine
              (src.GetLineString(body.Range.EndLine - 1).Length))
        let declText = src.GetSubTextFromRange fullRange
        let lines =
          declText.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
        let indent = pattern.Range.StartColumn
        let contentParts =
          lines
          |> Array.map (fun line -> line.Trim())
          |> Array.filter (fun line -> line <> "")
        let oneLine = String.concat " " contentParts
        let totalLength = indent + oneLine.Length
        if totalLength <= Utils.MaxLineLength then
          reportWarn src body.Range
            ("Unnecessary line break: declaration fits within 80 " +
             "columns")
      else
        ()
    | None ->
      ()

let checkComputationExprPlacement (src: ISourceText) (binding: SynBinding) =
  let SynBinding(expr = body; trivia = trivia) = binding
  if trivia.EqualsRange.IsSome then
    match body with
    | SynExpr.ComputationExpr _
    | SynExpr.App(argExpr = SynExpr.ComputationExpr _)
      when trivia.EqualsRange.Value.EndLine = body.Range.StartLine ->
      reportWarn src body.Range
        ("Computation expression should start on the next line after " +
         "'='")
    | _ ->
      ()
  else
    ()

let check (src: ISourceText) decls =
  decls
  |> List.pairwise
  |> List.iter (fun (prevDecl: SynModuleDecl, nextDecl) ->
    if prevDecl.IsLet && nextDecl.IsLet then
      let expectedSpacing = calculateSpacingBetweenDecls src prevDecl nextDecl
      let actualSpacing =
        nextDecl.Range.StartLine - prevDecl.Range.EndLine
        |> adjustByComment src prevDecl.Range nextDecl.Range expectedSpacing
      if actualSpacing <> expectedSpacing then
        reportWarn src nextDecl.Range "Wrong newLine appear."
      else
        ()
    else
      ()
  )