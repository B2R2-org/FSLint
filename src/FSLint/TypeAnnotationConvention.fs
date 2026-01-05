module B2R2.FSLint.TypeAnnotation

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open CustomReports
open Diagnostics

let private checkColonSpace src (patRange: range) typeLen (range: range) =
  if range.EndColumn <> patRange.EndColumn + typeLen + 2 then
    reportTypeError src range
  else
    let pat = src.GetSubTextFromRange range
    let colon = pat.Substring(patRange.EndColumn - patRange.StartColumn, 1)
    if colon <> ":" then reportTypeError src range else ()

let private checkTupleSpace src (path: SynTupleTypeSegment list) =
  let checkArray = function
    | SynTupleTypeSegment.Type(SynType.Array(_, elementType, typRange)) ->
      if elementType.Range.EndColumn + 2 <> typRange.EndColumn
      then reportTypeError src typRange
      else ()
    | _ ->
      ()
  path
  |> List.windowed 3
  |> List.filter (function
    | [ element1; element2; element3 ]
      when element1.IsType && element2.IsStar && element3.IsType -> true
      | _ -> false
  )
  |> List.iter (fun typeSegment ->
    checkArray typeSegment[0]
    checkArray typeSegment[2]
    if typeSegment[0].Range.EndLine <> typeSegment[1].Range.StartLine then
      if typeSegment[2].Range.StartColumn - 1 <> typeSegment[1].Range.EndColumn
      then reportTypeError src typeSegment[1].Range
      else ()
    elif typeSegment[1].Range.EndLine <> typeSegment[2].Range.StartLine then
      if typeSegment[1].Range.StartColumn - 1 <> typeSegment[0].Range.EndColumn
      then reportTypeError src typeSegment[1].Range
      else ()
    elif typeSegment[0].Range.EndColumn + 1 <> typeSegment[1].Range.StartColumn
      || typeSegment[2].Range.StartColumn - 1 <> typeSegment[1].Range.EndColumn
    then reportTypeError src typeSegment[1].Range
    else ()
  )

let private checkArraySpace (src: ISourceText) (typRange: range) =
  if (src.GetSubTextFromRange typRange).Contains " []" then
    reportTypeError src typRange
  else
    ()

(* TODO: Need to change usage AST *)
let checkFieldWidth (src: ISourceText) (range: range) =
  src.GetSubTextFromRange range
  |> fun originStr ->
    if range.StartLine <> range.EndLine then
      let strArr = originStr.Split([| '\n' |], StringSplitOptions.None)
      if Array.exists(fun (str: string) ->
           str.TrimStart().StartsWith "///") strArr then
        strArr
        |> Array.skipWhile (fun line -> line.TrimStart().StartsWith "///")
        |> Array.map (fun line -> line.TrimStart())
        |> String.Concat, true
      else originStr, false
    else originStr, true
  |> fun (str, executeCheck) ->
    if executeCheck then
      if str.TrimStart().Contains "  " then
        (* Heuristic: Assuming a standardized structure *)
        let indentSize = range.StartColumn + 2
        let colonIdx = str.IndexOf ':' + indentSize
        let spaceIdx = str.IndexOf "  " + indentSize
        let warnRange =
          (Position.mkPos range.StartLine spaceIdx,
           Position.mkPos range.StartLine colonIdx)
          ||> Range.mkRange ""
        reportWarn src warnRange "Remove consecutive whitespace"
      elif str.Contains ":" && str.Contains " :: " |> not
        && (str.Contains ": " |> not || str.Contains " :") then
        if str.Contains "::" then
          reportWarn src range "Use ': '"
        else
          let indentSize = range.StartColumn
          let startIdx = str.IndexOf ':' + indentSize
          let endIdx =
            str[startIdx..].IndexOf(str[startIdx..].TrimStart()[0]) + startIdx
            + indentSize
          Range.mkRange "" (Position.mkPos range.StartLine startIdx)
            (Position.mkPos range.StartLine endIdx)
          |> fun rangeCreated -> reportWarn src rangeCreated "Use ': '"
      elif str.Contains "*" && str.Contains " * " |> not then
        reportWarn src range "Use ' * '"
      elif str.Contains " []" && str.Contains ": []" |> not
        && str.Contains ", []" |> not then
        let idx = str.IndexOf " []"
        (Position.mkPos range.StartLine (range.StartColumn + idx),
         Position.mkPos range.StartLine (range.StartColumn + idx + 1))
        ||> Range.mkRange ""
        |> fun wRange -> reportWarn src wRange "Remove whitespace before '[]'"
      else
        ()
    else
      let strArr = str.Split([| '\n' |], StringSplitOptions.None)
      strArr
      |> Array.iter (fun str ->
        if str.TrimStart().Contains "  " then
          reportWarn src range "Remove consecutive whitespace"
        else
          ())
      if strArr[0].Contains ":" && strArr[0].Contains " :"
        && strArr[0].Contains " :: " |> not then
        reportWarn src range "Use ': ' or ' :: ' "
      else
        ()

let private checkFieldsWidth (src: ISourceText) (fields: SynField list) =
  fields
  |> List.map (fun field -> field.Range)
  |> List.pairwise
  |> List.iter (fun (front, back) ->
    Range.mkRange "" front.End back.Start
    |> fun range ->
      let str = src.GetSubTextFromRange range
      if front.StartLine = back.StartLine && str.Contains "  " then
        reportWarn src back "Remove consecutive whitespace"
      elif front.StartLine = back.StartLine && str.Contains " * " |> not
        && front.EndLine = back.StartLine then
        reportWarn src range "Use ' * '"
      elif front.StartLine <> back.StartLine && str.Contains "* " |> not
        && front.EndLine = back.StartLine then
        reportWarn src range "Use ' * '"
      else
        ()
  )
  fields

let private checkInlineSpacing src (frontCase, endCase) =
  let SynUnionCase(caseType = frontCaseType) = frontCase
  let SynUnionCase(range = endRange; trivia = endTrivia) = endCase
  match endTrivia.BarRange, frontCaseType with
  | Some barRange, SynUnionCaseKind.Fields fields when not fields.IsEmpty ->
    let lastFieldEnd = (List.last fields).Range.EndColumn
    let expectedBarStart = lastFieldEnd + 1
    let expectedCaseStart = barRange.EndColumn + 1
    if barRange.StartColumn <> expectedBarStart ||
      endRange.StartColumn <> expectedCaseStart
    then reportWarn src endRange "Use single whitespace around '|'"
    else ()
  | None, _ ->
    warn "Exception: '|' range does not exist"
  | _ ->
    ()

let checkSynFields src fields =
  fields
  |> checkFieldsWidth src
  |> List.iter (fun field -> checkFieldWidth src field.Range)

let private checkFieldsInUnion src case =
  let SynUnionCase(caseType = caseType) = case
  match caseType with
  | SynUnionCaseKind.Fields fields -> checkSynFields src fields
  | _ -> ()

/// Checks spacing in union case fields like `X of a: int * b: int`.
/// Ensures correct spacing around `:` and `*` in `a: int * b: int`.
let checkUnionType (src: ISourceText) (cases: SynUnionCase list) =
  cases |> List.iter (checkFieldsInUnion src)
  match cases with
  | _ when List.length cases > 1 &&
           cases.Head.Range.StartLine = (List.last cases).Range.StartLine ->
    cases |> List.pairwise |> List.iter (checkInlineSpacing src)
  | _ -> ()

let checkMember src (id: Ident) (typ: option<SynType>) =
  match typ with
  | Some(SynType.Tuple(path = path)) ->
    checkTupleSpace src path
  | Some(SynType.Array(range = range)) ->
    Range.mkRange "" id.idRange.Start range.End
    |> checkFieldWidth src
  | Some(SynType.LongIdent(longDotId = SynLongIdent(id = typeId))) ->
    Range.mkRange "" id.idRange.Start (List.last typeId).idRange.End
    |> checkFieldWidth src
  | _ -> ()

let rec checkAbstractSlot src (id: Ident) synType =
  match synType with
  | SynType.Fun(argType = argType; returnType = returnType; trivia = trivia) ->
    if argType.Range.EndLine <> trivia.ArrowRange.StartLine
      && returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn
    then reportWarn src trivia.ArrowRange "Use single whitespace around '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine = returnType.Range.StartLine
      && (argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
      || returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn)
    then reportWarn src trivia.ArrowRange "Use single whitespace around '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine <> returnType.Range.StartLine
      && argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
    then reportWarn src trivia.ArrowRange "Use single whitespace around '->'"
    else ()
    checkAbstractSlot src id argType
    checkAbstractSlot src id returnType
  | SynType.Tuple(path = path) ->
    checkTupleSpace src path
  | SynType.Array(range = range) ->
    Range.mkRange "" id.idRange.Start range.End
    |> checkFieldWidth src
  | SynType.LongIdent(longDotId = SynLongIdent(id = typeId)) ->
    Range.mkRange "" id.idRange.Start (List.last typeId).idRange.End
    |> checkFieldWidth src
  | _ -> ()

let checkReturnInfo (src: ISourceText) pat returnInfo =
  match returnInfo with
  | Some(SynBindingReturnInfo(typeName = typeName; trivia = trivia)) ->
    if trivia.ColonRange.IsSome then
      Range.mkRange "" (pat: SynPat).Range.End typeName.Range.End
      |> checkFieldWidth src
    else
      ()
  | _ -> ()

let checkFunction src (pat: SynSimplePat) (typ: SynType) =
  Range.mkRange "" pat.Range.Start typ.Range.End |> checkFieldWidth src

let checkPat src (pat: SynPat) range = function
  | SynType.LongIdent(SynLongIdent([ id ], _, _)) ->
    checkColonSpace src pat.Range id.idText.Length range
  | SynType.LongIdent(SynLongIdent([ id1; id2 ], _, _)) ->
    let typeLen = id1.idText.Length + id2.idText.Length + 1
    checkColonSpace src pat.Range typeLen range
  | SynType.App(_, _, _, _, _, _, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.Array(_, _, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkArraySpace src typRange
    checkColonSpace src pat.Range typeLen range
  | SynType.Var(_, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.Fun(_, _, typRange, _) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.HashConstraint(_, typRange) ->
    let typeLen = typRange.EndColumn - typRange.StartColumn
    checkColonSpace src pat.Range typeLen range
  | SynType.Tuple(path = path) ->
    checkTupleSpace src path
  | typ -> warn $"TODO: [Type Annotation] {typ}"
