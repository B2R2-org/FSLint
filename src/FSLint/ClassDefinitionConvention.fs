module B2R2.FSLint.ClassDefinition

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let private extractTypeNameRange decl =
  let SynTyparDecl(typar = SynTypar(ident = ident)) = decl
  ident.idRange

let private findIdxRange fileName lineNumber startCol endColExclusive line =
  let rec loop pos acc =
    if pos <= endColExclusive - 3 then
      if (line: string).Substring(pos, 3) = "\"\"\"" then
        let tripleQuoteRange =
          Range.mkRange fileName (Position.mkPos lineNumber pos)
            (Position.mkPos lineNumber (pos + 3))
        loop (pos + 3) (tripleQuoteRange :: acc)
      else
        loop (pos + 1) acc
    else
      List.rev acc
  loop startCol []

let rec private unionRange acc ranges =
  match ranges with
  | startRange :: endRange :: rest ->
    unionRange (Range.unionRanges startRange endRange :: acc) rest
  | _ ->
    List.rev acc

let getTripleQuoteRange (src: ISourceText) (range: range) =
  [ range.StartLine .. range.EndLine ]
  |> List.collect (fun lineNumber ->
    let line = src.GetLineString(lineNumber - 1)
    let startCol = if lineNumber = range.StartLine then range.StartColumn else 0
    let endCol =
      if lineNumber = range.EndLine then min range.EndColumn line.Length
      else line.Length
    if endCol - startCol < 3 then []
    else findIdxRange range.FileName lineNumber startCol endCol line
  )
  |> unionRange []
  |> List.map (fun r -> [ r.StartLine .. r.EndLine ])
  |> List.filter (fun ranges -> ranges.Length > 1)
  |> List.concat

let checkMultiLineIdentWithParen (src: ISourceText) ctorRange spaceRange =
  src.GetLineString((ctorRange: range).StartLine - 1)
  |> fun str ->
    if src.GetSubTextFromRange(ctorRange).TrimStart()[0] = str.TrimStart()[0]
    then
      ()
    else
      let subStr = str.Substring(ctorRange.StartColumn - 1, 2)
      if subStr.StartsWith ' ' then reportPascalCaseError src spaceRange else ()

let checkIdentifierWithParen (src: ISourceText) members =
  members
  |> List.iter (fun memberDefn ->
    match memberDefn with
    | SynMemberDefn.ImplicitCtor(accessibility = accessibility
                                 ctorArgs = ctorArgs
                                 range = range) ->
      match accessibility with
      | Some(SynAccess.Internal idRange)
      | Some(SynAccess.Public idRange)
      | Some(SynAccess.Private idRange) ->
        if idRange.EndColumn <> ctorArgs.Range.StartColumn then
          Range.mkRange "" idRange.End ctorArgs.Range.Start
          |> fun wRange -> reportPascalCaseError src wRange
        else
          ()
        TypeAnnotation.checkParamTypeSpacing src ctorArgs
      | _ ->
        Range.mkRange "" range.End ctorArgs.Range.Start
        |> checkMultiLineIdentWithParen src ctorArgs.Range
    | SynMemberDefn.ImplicitInherit(inheritType = inheritType
                                    inheritArgs = inheritArgs) ->
      if inheritType.Range.EndColumn <> inheritArgs.Range.StartColumn then
        Range.mkRange "" inheritType.Range.End inheritArgs.Range.Start
        |> fun wRange -> reportPascalCaseError src wRange
      else
        ()
    | SynMemberDefn.Inherit(baseType = baseType; asIdent = ident)
      when baseType.IsSome && ident.IsSome ->
      if baseType.Value.Range.EndColumn <> ident.Value.idRange.StartColumn then
        Range.mkRange "" baseType.Value.Range.End ident.Value.idRange.Start
        |> fun wRange -> reportPascalCaseError src wRange
      else
        ()
    | _ -> ()
  )

let checkAttributesLineSpacing src (attribute: SynAttributes) trivia =
  let lastAttr = List.tryLast attribute
  if Option.isNone lastAttr then ()
  else
    match (trivia: SynTypeDefnTrivia).LeadingKeyword with
    | SynTypeDefnLeadingKeyword.StaticType(typeRange = range)
    | SynTypeDefnLeadingKeyword.Type range ->
      if lastAttr.Value.Range.EndLine + 1 <> range.StartLine
        && lastAttr.Value.Range.StartLine <> range.StartLine then
        Range.mkRange "" (Position.mkPos (range.StartLine - 1) 0) range.Start
        |> reportNewLine src
      else
        ()
    | _ ->
      ()

let checkNameBracketSpacing src (idRange: range) (innerRange: range) =
  if idRange.EndColumn <> innerRange.StartColumn then
    Range.mkRange "" idRange.End innerRange.Start |> reportLeftAngleSpacing src
  else
    ()

let checkBracketElementSpacingInTypar (src: ISourceText) decls =
  if (decls: list<SynTyparDecl>).Length > 1 then
    decls
    |> List.map extractTypeNameRange
    |> List.pairwise
    |> List.iter (fun (front, back) ->
      let gap = Range.mkRange "" front.End back.Start
      let gapStr = gap |> src.GetSubTextFromRange
      if back.StartColumn - 2 <> front.EndColumn
        && front.EndLine = back.StartLine then
        if gapStr.StartsWith "," then
          Range.mkRange "" (Position.mkPos front.EndLine (front.EndColumn + 1))
            back.Start
          |> reportCommaAfterSpacing src
        else
          reportCommaFormat src gap
      else
        ()
    )
  else
    ()

let checkBracketSpacingInTypar src decls constraints (range: range) =
  let innerRange =
    if (constraints: List<SynTypeConstraint>).Length <> 0 then
      Range.unionRanges (List.head decls |> extractTypeNameRange)
        (List.last constraints).Range.EndRange
    else
      Range.unionRanges (List.head decls |> extractTypeNameRange)
        (List.last decls |> extractTypeNameRange)
  if range.StartLine = innerRange.StartLine
    && range.StartColumn + 1 <> innerRange.StartColumn then
    Range.mkRange "" (Position.mkPos range.StartLine (range.StartColumn + 1))
      innerRange.Start |> reportLeftAngleInnerSpacing src
  elif range.EndLine = innerRange.EndLine
    && innerRange.EndColumn + 1 <> range.EndColumn then
    Range.mkRange "" innerRange.End range.End
    |> reportRightAngleInnerSpacing src
  else
    ()

let checkSynTypar src idRange (typeParams: SynTyparDecls) =
  match typeParams with
  | SynTyparDecls.PostfixList(decls = decls
                              constraints = constraints
                              range = range) ->
    checkNameBracketSpacing src idRange range
    checkBracketElementSpacingInTypar src decls
    checkBracketSpacingInTypar src decls constraints range
  | _ -> warn "[checkSynTypar] TODO"

let checkNestedTypeDefns (src: ISourceText) (range: range) typeDefns =
  if isStrict then
    typeDefns
    |> List.skip 1
    |> List.map (fun (typeDefn: SynTypeDefn) -> typeDefn.Range.StartLine - 1)
    |> List.iter (fun recurseIdx ->
      if src.GetLineString(recurseIdx - 1) <> ""
      then reportWarn src range "Add blank line before nested"
      else ()
    )
  else
    ()

let checkLineBreak src range =
  let tripleQuoteLines = getTripleQuoteRange src range |> Set.ofList
  let findMultiline src acc lineIdx =
    if Set.contains lineIdx tripleQuoteLines then
      0
    elif isBlankLine src lineIdx then
      if acc >= 1 then
        Range.mkRange range.FileName
          (Position.mkPos (lineIdx - 1) 0)
          (Position.mkPos (lineIdx - 1) 1)
        |> fun range -> reportWarn src range "Use at most single blank line"
        acc + 1
      else
        acc + 1
    else
      0
  [ range.StartLine .. range.EndLine ]
  |> List.fold (fun acc lineIdx -> findMultiline src acc lineIdx) 0
  |> ignore