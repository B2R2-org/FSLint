module B2R2.FSLint.ClassMemberConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FunctionCallConvention

/// Check the spacing around backtick-enclosed method names in F# source code.
let checkBackticMethodSpacing (src: ISourceText) dotRanges (parenRange: range) =
  if (dotRanges: range list).Length = 1 then
    let dotRange = List.head dotRanges
    (Position.mkPos dotRange.StartLine dotRange.EndColumn,
    Position.mkPos dotRange.StartLine (dotRange.EndColumn + 2))
    ||> Range.mkRange ""
    |> src.GetSubTextFromRange
    |> fun str ->
      let lineStr = src.GetLineString(dotRange.StartLine - 1)
      if str = "``" then
        if lineStr.LastIndexOf "``" + 2 <> parenRange.StartColumn then
          reportError src dotRange "Contains invalid whitespace"
        else ()
        false
      else true
  else true

/// Check the spacing between identifiers and parentheses based on their casing.
/// If either identifier is in PascalCase, no space between the identifier and
/// the opening parenthesis. If in lower case, ensures exactly one space exists.
let checkMemberSpacing (src: ISourceText) longId extraId dotRanges args =
  match (longId: LongIdent) with
  | id :: _ when id.idText = "this" || id.idText = "_" ->
    match args with
    | SynPat.Wild _ :: wild when wild.Length <> 0 ->
      reportError src id.idRange "Member must be followed by paren."
    | SynPat.Paren _ :: paren when paren.Length <> 0 ->
      reportError src id.idRange "Member must be followed by paren."
    | SynPat.Named _ :: named when named.Length <> 0 ->
      reportError src id.idRange "Member must be followed by paren."
    | [ SynPat.Paren(range = range) ] ->
      let lastId = List.last longId
      if checkBackticMethodSpacing src dotRanges range then
        if (extraId: Ident Option).IsSome
          && isPascalCase extraId.Value.idText then
          if extraId.Value.idRange.EndColumn <> range.StartColumn then
            reportPascalCaseError src extraId.Value.idRange
          else ()
        elif (extraId: Ident Option).IsSome then
          if extraId.Value.idRange.EndColumn + 1 <> range.StartColumn then
            reportLowerCaseError src extraId.Value.idRange
          else ()
        elif isPascalCase lastId.idText
          && lastId.idRange.EndColumn <> range.StartColumn
          then reportPascalCaseError src lastId.idRange
        elif isPascalCase lastId.idText |> not
          && lastId.idRange.EndColumn + 1 <> range.StartColumn
          then reportLowerCaseError src lastId.idRange
        else ()
      else ()
    | _ -> ()
  | _ -> ()

/// Checks spacing between static member identifiers and parentheses in F# code.
/// For PascalCase members, ensures no space before the parenthesis.
/// For infix notation with parentheses.
let checkStaticMemberSpacing src (longId: LongIdent) args idTrivia =
  match longId with
  | [ id ] when (idTrivia: list<option<IdentTrivia>>).Head.IsNone ->
    match args with
    | SynPat.Wild _ :: wild when wild.Length <> 0 ->
      reportError src id.idRange "Static member must be followed by paren."
    | SynPat.Paren _ :: paren when paren.Length <> 0 ->
      reportError src id.idRange "Static member must be followed by paren."
    | SynPat.Named _ :: named when named.Length <> 0 ->
      reportError src id.idRange "Static member must be followed by paren."
    | [ SynPat.Paren(range = range) ] ->
      if id.idRange.EndColumn <> range.StartColumn then
        reportPascalCaseError src id.idRange
      else ()
    | _ -> ()
  | _ when (idTrivia: list<option<IdentTrivia>>).Head.IsSome ->
    match idTrivia.Head.Value with
    | IdentTrivia.OriginalNotationWithParen(rightParenRange = range) ->
      match args with
      | [ SynPat.Paren(range = argRange) ] ->
        if range.EndColumn + 1 <> argRange.StartColumn then
          reportError src argRange "Infix and Paren need a single space."
        else ()
      | _ -> ()
    | _ -> warn $"[checkStaticMemberSpacing]TODO: {longId}"
  | _ -> ()