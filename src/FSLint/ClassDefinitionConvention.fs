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
      LineBreakConvention.checkParameters src [ ctorArgs ]
      TypeAnnotation.checkParamTypeSpacing src ctorArgs
      match accessibility with
      | Some(SynAccess.Internal idRange)
      | Some(SynAccess.Public idRange)
      | Some(SynAccess.Private idRange) ->
        if idRange.EndColumn <> ctorArgs.Range.StartColumn then
          Range.mkRange "" idRange.End ctorArgs.Range.Start
          |> fun wRange -> reportPascalCaseError src wRange
        else
          ()
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
    | _ ->
      ()
  )

let checkAttributesLineSpacing src (attribute: SynAttributes) trivia =
  let lastAttr = List.tryLast attribute
  if Option.isNone lastAttr then
    ()
  else
    match (trivia: SynTypeDefnTrivia).LeadingKeyword with
    | SynTypeDefnLeadingKeyword.StaticType(typeRange = range)
    | SynTypeDefnLeadingKeyword.Type range ->
      (* A directive standing between the attribute and its type holds the two
         apart, and its own line cannot be taken away to close the gap. The
         attribute is as near its type as it is allowed to be. *)
      let heldApart =
        findDirectivesBetween lastAttr.Value.Range range |> Option.isSome
      if lastAttr.Value.Range.EndLine + 1 <> range.StartLine
        && lastAttr.Value.Range.StartLine <> range.StartLine
        && not heldApart then
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

/// The `when` or `and` standing ahead of a constraint on its line: where the
/// keyword sits, and whether it opens that line. Neither keyword is part of
/// the constraint the tree gives back, so the line ahead of it is read.
let private precedingKeyword (src: ISourceText) (range: range) =
  let line = src.GetLineString(range.StartLine - 1)
  let before = line.Substring(0, range.StartColumn).TrimEnd()
  let name =
    if before.EndsWith "when" then "when"
    elif before.EndsWith "and" then "and"
    else ""
  if name = "" then
    None
  else
    let column = before.Length - name.Length
    let keyword =
      Range.mkRange "" (Position.mkPos range.StartLine column)
        (Position.mkPos range.StartLine before.Length)
    Some(keyword, column, before.TrimStart() = name)

/// The constraints of a type parameter list are a separator list like any
/// other: `when` opens it and `and` divides it. Either the whole list keeps to
/// the line the parameters are on, or `when` starts a line of its own and each
/// constraint takes one after it, every `and` standing in the column `when`
/// opened.
///
/// A `when` still up on the parameter line is the only thing said of such a
/// list. Sending it down takes the constraints below it along, and what the
/// `and`s under it are doing cannot be judged until it lands.
///
/// Where that column falls is not asked. `when` and `and` are the one pair of
/// separators in the language of unequal length, so a column can hold the
/// keywords or the constraints but not both, and it is the keywords that are
/// held; how far in they sit is left to the author.
let checkTyparConstraints src (constraints: SynTypeConstraint list) range =
  if not isStrict || constraints.IsEmpty then
    ()
  elif (range: range).StartLine = range.EndLine then
    ()
  else
    match constraints with
    | head :: rest ->
      match precedingKeyword src head.Range with
      | None ->
        reportWhenPlacement src head.Range
      | Some(keyword, _, false) ->
        reportWhenPlacement src keyword
      | Some(_, column, true) ->
        rest
        |> List.iter (fun constr ->
          match precedingKeyword src constr.Range with
          | Some(_, found, true) when found = column -> ()
          | Some(keyword, _, _) -> reportAndAlignment src keyword
          | None -> reportAndAlignment src constr.Range
        )
    | [] ->
      ()

let checkSynTypar src idRange (typeParams: SynTyparDecls) =
  match typeParams with
  | SynTyparDecls.PostfixList(decls = decls
                              constraints = constraints
                              range = range) ->
    checkNameBracketSpacing src idRange range
    checkBracketElementSpacingInTypar src decls
    checkBracketSpacingInTypar src decls constraints range
    checkTyparConstraints src constraints range
    let declRanges = decls |> List.map extractTypeNameRange
    (* The parameters are measured by their own width, not by the fence they
       share with the constraints. A constraint list too wide for the line says
       nothing about whether the parameters ahead of it fit on one, and reading
       the two together would leave them broken with nothing gained. *)
    let declSpan =
      if constraints.IsEmpty then range
      else Range.mkRange "" range.Start (List.last declRanges).End
    LineBreakConvention.checkBracketedPlacement src declSpan declRanges
  | _ ->
    warn "[checkSynTypar] TODO"

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