module B2R2.FSLint.TypeAnnotation

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics
open TypeUseConvention

let rec private collectArraysRange currentType ranges =
  match currentType with
  | SynType.Array(_, innerType, range) ->
    collectArraysRange innerType (range :: ranges)
  | _ ->
    List.rev ranges

let checkColonSpace src (idRange: range) (sigRange: range) =
  let gap = sigRange.StartColumn - idRange.EndColumn
  if gap >= 0 then
    let patIdentRange = Range.mkRange "" idRange.End sigRange.Start
    let str = patIdentRange |> (src: ISourceText).GetSubTextFromRange
    if sigRange.StartLine = idRange.StartLine then
      if str.StartsWith ':' && gap <> 2 then
        Range.mkRange ""
          (Position.mkPos idRange.StartLine (idRange.EndColumn + 1))
          sigRange.Start
        |> fun range -> reportWarn src range "Use single whitespace after ':'"
      elif gap = 0 || str.EndsWith " :" then
        Range.mkRange "" idRange.End
          (Position.mkPos idRange.StartLine (sigRange.StartColumn - 1))
        |> fun range -> reportWarn src range "Remove whitespace before ':'"
      elif gap <> 2 then
        reportWarn src patIdentRange "Use ': '"
    else
      ()
  else
    ()

let private checkGapBetweenArrays src (ranges: list<range>) =
  ranges
  |>List.rev
  |> List.pairwise
  |> List.iter (fun (front, back) ->
    if back.EndColumn - front.EndColumn <> 2 then
      Range.mkRange "" front.End
        (Position.mkPos back.StartLine (back.EndColumn - 2))
      |> fun range -> reportWarn src range "Remove whitespace around '[]'"
    else
      ()
  )

let rec private checkTupleSpacing src path =
  match path with
  | SynTupleTypeSegment.Type synType1 ::
    SynTupleTypeSegment.Star starRange ::
    SynTupleTypeSegment.Type synType2 :: rest ->
    if synType1.Range.EndLine = starRange.StartLine
      && synType1.Range.EndColumn + 1 <> starRange.StartColumn then
      Range.mkRange "" synType1.Range.End starRange.Start
      |> fun range -> reportWarn src range "Use ' * '"
    elif starRange.EndLine = synType2.Range.StartLine
      && starRange.EndColumn + 1 <> synType2.Range.StartColumn then
      Range.mkRange "" synType2.Range.Start starRange.End
      |> fun range -> reportWarn src range "Use ' * '"
    else
      ()
    checkTypeInternal src synType1
    checkTupleSpacing src (SynTupleTypeSegment.Type synType2 :: rest)
  | SynTupleTypeSegment.Type synType :: rest ->
    checkTypeInternal src synType
    checkTupleSpacing src rest
  | _ :: rest ->
    checkTupleSpacing src rest
  | [] -> ()

and checkArray src = function
  | SynType.Array(elementType = SynType.LongIdent(longDotId = id)
                  range = range) ->
    if id.Range.EndColumn + 2 <> range.EndColumn then
      Range.mkRange "" id.Range.End range.End
      |> fun range -> reportWarn src range "Remove whitespace in array type"
    else
      ()
  | _ -> ()

and checkExprToLessSpacing src (typeName: SynType) (lessRange: option<Range>) =
  if typeName.Range.EndLine = lessRange.Value.StartLine
    && typeName.Range.EndColumn <> lessRange.Value.StartColumn then
    Range.mkRange "" typeName.Range.End lessRange.Value.Start
    |> fun range -> reportWarn src range "Remove whitespace before '<'"
  else
    ()

and checkTypeInternal src synType =
  checkArray src synType
  match synType with
  | SynType.Array(elementType = elementType; range = range) ->
    collectArraysRange elementType [ range ] |> checkGapBetweenArrays src
    checkArray src elementType
    checkTypeInternal src elementType
  | SynType.Tuple(path = path) ->
    checkTupleSpacing src path
  | SynType.Paren(innerType = inner; range = range) ->
    if range.StartColumn + 1 <> inner.Range.StartColumn then
      Range.mkRange "" range.Start inner.Range.Start
      |> fun range -> reportWarn src range "Remove whitespace after '('"
    elif inner.Range.EndColumn + 1 <> range.EndColumn then
      Range.mkRange "" inner.Range.End range.End
      |> fun range -> reportWarn src range "Remove whitespace before ')'"
    else
      ()
    checkTypeInternal src inner
  | SynType.Fun(argType = argType; returnType = returnType) ->
    checkTypeInternal src argType
    checkTypeInternal src returnType
  | SynType.SignatureParameter(usedType = usedType) ->
    checkTypeInternal src usedType
  | SynType.App(typeName = typeName
                lessRange = lessRange
                greaterRange = greaterRange
                typeArgs = typeArgs)
    when lessRange.IsSome && greaterRange.IsSome ->
    checkExprToLessSpacing src typeName lessRange
    collectRangeOfFirstAndLastType typeArgs
    |> checkBracketRanges src lessRange greaterRange
    checkTypeElementSpacing src typeArgs
    List.iter (checkTypeInternal src) typeArgs
  | _ -> ()

let checkFieldWidth (src: ISourceText) field =
  let SynField(idOpt = idOpt; fieldType = fieldType) = field
  if Option.isSome idOpt then
    Range.unionRanges idOpt.Value.idRange.EndRange fieldType.Range.StartRange
    |> fun gap ->
      if src.GetSubTextFromRange gap <> ": " then reportWarn src gap "Use ': '"
      else ()
    collectArraysRange fieldType [] |> checkGapBetweenArrays src
    checkTypeInternal src fieldType
  else
    collectArraysRange fieldType [] |> checkGapBetweenArrays src
    checkTypeInternal src fieldType

let getFieldDeclaration (src: ISourceText) (field: SynField) =
  let SynField(idOpt = idOpt; fieldType = fieldType) = field
  match idOpt with
  | Some ident ->
    let declRange = Range.unionRanges ident.idRange fieldType.Range
    src.GetSubTextFromRange declRange
    |> fun text ->
      text.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
      |> Array.filter (fun line -> not (line.TrimStart().StartsWith "///"))
      |> String.concat " "
      |> fun s -> s.Trim()
  | None -> ""

let private checkFieldsWidth (src: ISourceText) (fields: SynField list) =
  try
    fields
    |> List.map (fun field -> field.Range)
    |> List.pairwise
    |> List.iter (fun (front, back) ->
      Range.mkRange "" front.End back.Start
      |> fun range ->
        let str = src.GetSubTextFromRange range
        let lastElemToDoubleCol =
          let frontStr = src.GetLineString(front.StartLine - 1)
          if frontStr.Length - front.EndColumn > 1 then
            Range.mkRange "" front.End
              (Position.mkPos front.StartLine (front.EndColumn + 2))
            |> src.GetSubTextFromRange
          else
            Range.mkRange "" front.End
              (Position.mkPos front.StartLine frontStr.Length)
            |> src.GetSubTextFromRange
        if front.StartLine = back.StartLine
          && back.StartColumn - front.EndColumn > 3
        then
          let starIndex = str.IndexOf('*')
          let before = str.Substring(0, starIndex)
          let after = str.Substring(starIndex + 1)
          let leftSpaces = before.Length - before.TrimEnd().Length
          let rightSpaces = after.Length - after.TrimStart().Length
          if leftSpaces = 1 && rightSpaces = 1 then ()
          elif leftSpaces > rightSpaces then
            Range.mkRange "" front.End
              (Position.mkPos front.StartLine (front.EndColumn + leftSpaces))
            |> fun range -> reportWarn src range "Remove consecutive whitespace"
          else
            Range.mkRange ""
              (Position.mkPos back.StartLine (back.StartColumn - rightSpaces))
              back.Start
            |> fun range -> reportWarn src range "Remove consecutive whitespace"
        elif front.StartLine = back.StartLine && str.Contains " * " |> not
          && front.EndLine = back.StartLine
        then
          Range.mkRange "" front.End back.Start
          |> fun range -> reportWarn src range "Use ' * '"
        elif front.StartLine <> back.StartLine && str.Contains "* " |> not
          && front.EndLine = back.StartLine
        then
          Range.mkRange "" front.End back.Start
          |> fun range -> reportWarn src range "Use ' * '"
        elif front.StartLine <> back.StartLine
          && lastElemToDoubleCol.StartsWith "*"
        then
          Range.mkRange "" front.End
            (Position.mkPos front.StartLine (front.EndColumn + 1))
          |> fun range -> reportWarn src range "Use ' *'"
        elif front.StartLine <> back.StartLine
          && lastElemToDoubleCol <> " *" && lastElemToDoubleCol <> ""
        then
          let endIdx = str.TrimEnd().IndexOf '*' + front.EndColumn
          Range.mkRange "" front.End (Position.mkPos front.StartLine endIdx)
          |> fun range -> reportWarn src range "Use ' *'"
        else
          ()
    )
  with _ ->
    fields
    |> List.iter (fun field ->
      let fieldDecl = getFieldDeclaration src field
      if fieldDecl <> "" then
        if fieldDecl.Contains "  " then
          reportWarn src field.Range "Remove consecutive whitespace"
        elif fieldDecl.Contains "*" && not (fieldDecl.Contains " * ") then
          reportWarn src field.Range "Use ' * '"
    )

let private checkInlineSpacing src (frontCase, endCase) =
  let SynUnionCase(caseType = frontCaseType) = frontCase
  let SynUnionCase(range = endRange; trivia = endTrivia) = endCase
  match endTrivia.BarRange, frontCaseType with
  | Some barRange, SynUnionCaseKind.Fields fields when not fields.IsEmpty ->
    let lastFieldEnd = (List.last fields).Range.EndColumn
    let expectedBarStart = lastFieldEnd + 1
    let expectedCaseStart = barRange.EndColumn + 1
    if barRange.StartColumn <> expectedBarStart then
      Range.mkRange "" (Position.mkPos barRange.StartLine lastFieldEnd)
        barRange.Start
      |> fun range -> reportWarn src range "Use single whitespace before '|'"
    elif endRange.StartColumn <> expectedCaseStart
    then
      Range.mkRange "" endRange.Start
        (Position.mkPos endRange.StartLine barRange.EndColumn)
      |> fun range -> reportWarn src range "Use single whitespace after '|'"
    else ()
  | None, _ ->
    warn "Exception: '|' range does not exist"
  | _ ->
    ()

let checkSynFields src fields =
  fields |> checkFieldsWidth src
  fields |> List.iter (checkFieldWidth src)

let private checkFieldsInUnion src case =
  let SynUnionCase(caseType = caseType) = case
  match caseType with
  | SynUnionCaseKind.Fields fields ->
    checkSynFields src fields
  | SynUnionCaseKind.FullType(fullType = fullType) ->
    checkTypeInternal src fullType

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
  | Some(SynType.LongIdent(longDotId = SynLongIdent(id = typeId))) ->
    if typeId.Length = 1 then
      checkColonSpace src id.idRange typeId.Head.idRange
    else
      let ranges =
        Range.unionRanges typeId.Head.idRange (List.last typeId).idRange
      checkColonSpace src id.idRange ranges
  | Some typ ->
    checkTypeInternal src typ
  | _ ->
    ()

let extractColonPairs synType =
  let rec loop currentType acc =
    match currentType with
    | SynType.Fun(argType = arg; returnType = ret) ->
      let acc = loop arg acc
      loop ret acc
    | SynType.Tuple(path = path) ->
      loopPath path acc
    | SynType.Array(_, innerType, _) ->
      loop innerType acc
    | SynType.App(typeName = typeName; typeArgs = typeArgs) ->
      let acc = loop typeName acc
      List.fold (fun acc arg -> loop arg acc) acc typeArgs
    | SynType.Paren(innerType = inner) ->
      loop inner acc
    | SynType.HashConstraint(innerType, _) ->
      loop innerType acc
    | SynType.MeasurePower(baseMeasure, _, _) ->
      loop baseMeasure acc
    | SynType.StaticConstantExpr _
    | SynType.StaticConstantNamed _
    | SynType.Anon _
    | SynType.Var _
    | SynType.LongIdent _ ->
      acc
    | SynType.SignatureParameter(_, _, Some name, usedType, _) ->
      loop usedType ((name.idRange, usedType.Range) :: acc)
    | SynType.SignatureParameter(_, _, None, usedType, _) ->
      loop usedType acc
    | SynType.WithNull(innerType = innerType) ->
      loop innerType acc
    | synType ->
      warn $"[TypeAnnotation] Unhandled SynType case: {synType.GetType().Name}"
      acc
  and loopPath path acc =
    List.fold (fun acc seg ->
      match seg with
      | SynTupleTypeSegment.Type synType -> loop synType acc
      | _ -> acc) acc path
  loop synType [] |> List.rev

let rec checkTypeAbbrevWithAnnotation src = function
  | SynType.App(lessRange = lessRange
                typeArgs = typeArgs
                greaterRange = greaterRange) ->
    collectRangeOfFirstAndLastType typeArgs
    |> checkBracketRanges src lessRange greaterRange
    typeArgs |> List.iter (checkLongIdentSpacing src)
    checkTypeElementSpacing src typeArgs
  | SynType.Fun(argType = argType; returnType = returnType; trivia = trivia) ->
    if argType.Range.EndLine <> trivia.ArrowRange.StartLine
      && returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn
    then
      Range.mkRange "" trivia.ArrowRange.End returnType.Range.Start
      |> fun range -> reportWarn src range "Use single whitespace after '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine = returnType.Range.StartLine
      && argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
    then
      Range.mkRange "" argType.Range.End trivia.ArrowRange.Start
      |> fun range -> reportWarn src range "Use single whitespace before '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine = returnType.Range.StartLine
      && returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn
    then
      Range.mkRange "" trivia.ArrowRange.End returnType.Range.Start
      |> fun range -> reportWarn src range "Use single whitespace after '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine <> returnType.Range.StartLine
      && argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
    then
      Range.mkRange "" argType.Range.End trivia.ArrowRange.Start
      |> fun range -> reportWarn src range "Use single whitespace before '->'"
    else ()
    checkTypeInternal src argType
    checkTypeInternal src returnType
  | _ -> ()

let checkPat src (pat: SynPat) = function
  | SynType.LongIdent(SynLongIdent([ id ], _, _)) ->
    checkColonSpace src pat.Range id.idRange
  | SynType.LongIdent(SynLongIdent([ id1; id2 ], _, _)) ->
    Range.unionRanges id1.idRange id2.idRange
    |> checkColonSpace src pat.Range
  | SynType.App(range = range) ->
    checkColonSpace src pat.Range range
  | SynType.Array(elementType = elementType; range = range) as typ ->
    collectArraysRange elementType [ range ] |> checkGapBetweenArrays src
    checkArray src typ
    checkColonSpace src pat.Range range
  | SynType.Var(_, range) ->
    checkColonSpace src pat.Range range
  | SynType.Fun(_, _, range, _) ->
    checkColonSpace src pat.Range range
  | SynType.HashConstraint(_, range) ->
    checkColonSpace src pat.Range range
  | SynType.Tuple(path = path) ->
    checkTupleSpacing src path
  | typ -> warn $"TODO: [Type Annotation] {typ}"

let rec checkParamTypeSpacing src = function
  | SynPat.LongIdent(argPats = SynArgPats.NamePatPairs(pats = pats)) ->
    pats |> List.iter (fun field -> checkParamTypeSpacing src field.Pattern)
  | SynPat.LongIdent(argPats = SynArgPats.Pats pats) ->
    pats |> List.iter (checkParamTypeSpacing src)
  | SynPat.Tuple(elementPats = pats) ->
    pats |> List.iter (checkParamTypeSpacing src)
  | SynPat.Paren(pat, _) ->
    checkFieldWidthByPat src pat
    checkParamTypeSpacing src pat
  | SynPat.Typed(pat, targetType, range) as typed ->
    checkTypeAbbrevWithAnnotation src targetType
    checkFieldWidthByPat src typed
    checkParamTypeSpacing src pat
  | SynPat.As(lhsPat = lhsPat; rhsPat = rhsPat) ->
    checkParamTypeSpacing src lhsPat
    checkParamTypeSpacing src rhsPat
  | SynPat.IsInst(pat = pat) ->
    checkTypeAbbrevWithAnnotation src pat
  | _ ->
    ()

and checkFieldWidthByPat (src: ISourceText) pat =
  match pat with
  | SynPat.Paren(pat = pat) ->
    checkFieldWidthByPat src pat
  | SynPat.Typed(pat = pat; targetType = targetType) ->
    collectArraysRange targetType [] |> checkGapBetweenArrays src
    checkPat src pat targetType
    checkTypeInternal src targetType
  | SynPat.Tuple(elementPats = elementPats; commaRanges = commaRanges) ->
    TupleConvention.checkPat src elementPats commaRanges
    elementPats |> List.iter (checkFieldWidthByPat src)
  | _ ->
    ()

let checkExprAnnotation src = function
  | SynExpr.Typed(targetType = targetType) ->
    checkTypeAbbrevWithAnnotation src targetType
  | _ ->
    ()

let rec checkAbstractSlot src (id: Ident) (synType: SynType) =
  match synType with
  | SynType.Fun(argType = argType; returnType = returnType; trivia = trivia) ->
    if argType.Range.EndLine <> trivia.ArrowRange.StartLine
      && returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn
    then
      Range.mkRange "" trivia.ArrowRange.End returnType.Range.Start
      |> fun range -> reportWarn src range "Use single whitespace after '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine = returnType.Range.StartLine
      && argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
    then
      Range.mkRange "" argType.Range.End trivia.ArrowRange.Start
      |> fun range -> reportWarn src range "Use single whitespace before '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine = returnType.Range.StartLine
      && returnType.Range.StartColumn - 1 <> trivia.ArrowRange.EndColumn
    then
      Range.mkRange "" trivia.ArrowRange.End returnType.Range.Start
      |> fun range -> reportWarn src range "Use single whitespace after '->'"
    elif argType.Range.EndLine = trivia.ArrowRange.StartLine
      && trivia.ArrowRange.EndLine <> returnType.Range.StartLine
      && argType.Range.EndColumn + 1 <> trivia.ArrowRange.StartColumn
    then
      Range.mkRange "" argType.Range.End trivia.ArrowRange.Start
      |> fun range -> reportWarn src range "Use single whitespace before '->'"
    else
      ()
    checkAbstractSlot src id argType
    checkAbstractSlot src id returnType
  | SynType.Tuple(path = path) ->
    checkTupleSpacing src path
  | SynType.Array(elementType = elementType; range = range) ->
    collectArraysRange elementType [ range ] |> checkGapBetweenArrays src
    checkArray src elementType
    checkTypeInternal src elementType
  | SynType.App(typeName = typeName
                lessRange = lessRange
                greaterRange = greaterRange
                typeArgs = typeArgs)
    when lessRange.IsSome && greaterRange.IsSome ->
      checkExprToLessSpacing src typeName lessRange
      collectRangeOfFirstAndLastType typeArgs
      |> checkBracketRanges src lessRange greaterRange
      checkTypeElementSpacing src typeArgs
      List.iter (checkTypeInternal src) typeArgs
  | _ ->
    ()

and checkAbstractSpacing src (id: Ident) (synType: SynType) (keyRange: range) =
  if keyRange.EndColumn + 1 <> id.idRange.StartColumn then
    Range.mkRange "" keyRange.End id.idRange.Start
    |> fun range ->
      reportWarn src range "Use single whitespace between keyword and id"
  else
    ()
  checkColonSpace src id.idRange synType.Range
  match extractColonPairs synType with
  | [] -> ()
  | pairs ->
    for nameRange, typeRange in pairs do checkColonSpace src nameRange typeRange

let checkReturnInfo (src: ISourceText) (pat: SynPat) returnInfo =
  match returnInfo with
  | Some(SynBindingReturnInfo(typeName = typeName; range = range; trivia = tri))
    when tri.ColonRange.IsSome ->
      checkColonSpace src pat.Range range
      checkTypeInternal src typeName
  | _ ->
    ()

let checkFunction src (pat: SynSimplePat) (typ: SynType) =
  checkColonSpace src pat.Range typ.Range
  checkTypeInternal src typ