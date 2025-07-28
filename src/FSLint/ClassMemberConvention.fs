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
          if extraId.Value.idRange.EndColumn <> range.StartColumn then
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
let checkStaticMemberSpacing src (longId: LongIdent) typarDecls args idTrivia =
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
      let idRange =
        match typarDecls with
        | Some(SynValTyparDecls(typars = Some typars)) -> typars.Range
        | _ -> id.idRange
      if idRange.EndColumn <> range.StartColumn then
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

/// Traverses elements to find the presence of a self identifier.
/// Recursively searches through the given application structure,
/// accumulating results as it matches the specified pattern identifier.
let rec findSelfIdentifierInApp src patIdent acc = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = id))
    when not id.IsEmpty && id.Head.idText = patIdent -> true
  | SynExpr.For(ident = ident) when ident.idText = patIdent -> true
  | SynExpr.Ident(ident = ident) when ident.idText = patIdent -> true
  | SynExpr.Typed(expr = expr)
  | SynExpr.Assert(expr = expr)
  | SynExpr.Paren(expr = expr)
  | SynExpr.Downcast(expr = expr)
  | SynExpr.Upcast(expr = expr)
  | SynExpr.DotGet(expr = expr)
  | SynExpr.YieldOrReturn(expr = expr)
  | SynExpr.YieldOrReturnFrom(expr = expr)
  | SynExpr.ArrayOrListComputed(expr = expr) ->
    findSelfIdentifierInApp src patIdent acc expr
  | SynExpr.LetOrUseBang(rhs = expr1; body = expr2)
  | SynExpr.Sequential(expr1 = expr1; expr2 = expr2)
  | SynExpr.Set(targetExpr = expr1; rhsExpr = expr2)
  | SynExpr.DotSet(targetExpr = expr1; rhsExpr = expr2)
  | SynExpr.ForEach(enumExpr = expr1; bodyExpr = expr2)
  | SynExpr.Lambda(body = expr1; parsedData = Some(_, expr2))
  | SynExpr.App(funcExpr = expr1; argExpr = expr2) ->
    findSelfIdentifierInApp src patIdent acc expr1 ||
    findSelfIdentifierInApp src patIdent acc expr2
  | SynExpr.For(identBody = expr1; toBody = expr2; doBody = expr3) ->
    findSelfIdentifierInApp src patIdent acc expr1 ||
    findSelfIdentifierInApp src patIdent acc expr2 ||
    findSelfIdentifierInApp src patIdent acc expr3
  | SynExpr.Tuple(exprs = exprs) ->
    exprs |> List.exists (findSelfIdentifierInApp src patIdent acc)
  | SynExpr.MatchLambda(matchClauses = matchClauses) ->
    matchClauses
    |> List.exists (fun (SynMatchClause(whenExpr = whenExpr
                                        resultExpr = resultExpr)) ->
      findSelfIdentifierInApp src patIdent acc resultExpr ||
      if whenExpr.IsSome then
        findSelfIdentifierInApp src patIdent acc whenExpr.Value
      else false
    )
  | SynExpr.Match(expr = expr; clauses = clauses) ->
    clauses
    |> List.exists (fun (SynMatchClause(whenExpr = whenExpr
                                        resultExpr = resultExpr)) ->
      findSelfIdentifierInApp src patIdent acc expr ||
      findSelfIdentifierInApp src patIdent acc resultExpr ||
      if whenExpr.IsSome then
        findSelfIdentifierInApp src patIdent acc whenExpr.Value
      else false
    )
  | SynExpr.InterpolatedString(contents = contents)
    when not contents.IsEmpty && contents.Head.IsString ->
      contents
      |> List.exists (function
        | SynInterpolatedStringPart.FillExpr(expr, _) ->
          findSelfIdentifierInApp src patIdent acc expr
        | _ -> acc
      )
  | SynExpr.LetOrUse(bindings = bindings; body = body) ->
    bindings
    |> List.exists (fun (SynBinding(expr = expr)) ->
      findSelfIdentifierInApp src patIdent acc expr
    ) || findSelfIdentifierInApp src patIdent acc body
  | SynExpr.IfThenElse(ifExpr = ifExpr
                       thenExpr = thenExpr
                       elseExpr = elseExpr) ->
    findSelfIdentifierInApp src patIdent acc ifExpr ||
    findSelfIdentifierInApp src patIdent acc thenExpr ||
    if elseExpr.IsSome then
      findSelfIdentifierInApp src patIdent acc elseExpr.Value
    else false
  | _ -> acc

let checkSelfIdentifierUsage src pat body =
  match pat with
  | SynPat.LongIdent(longDotId = SynLongIdent(id = id))
    when not id.IsEmpty && id.Head.idText = "__" ->
      reportError src id.Head.idRange "Avoid usage '__'"
  | SynPat.LongIdent(longDotId = SynLongIdent(id = id))
    when not id.IsEmpty
      && (id.Head.idText = "this" || id.Head.idText = "self") ->
    if findSelfIdentifierInApp src id.Head.idText false body then ()
    else reportError src id.Head.idRange "Remove unused self-identifier"
  | _ -> ()