module B2R2.FSLint.ClassMemberConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics
open Utils

let private getMemberCategory (memberDefn: SynMemberDefn) =
  match memberDefn with
  | SynMemberDefn.ImplicitCtor _ -> MemberCategory.Constructor
  | SynMemberDefn.Member(binding, _) ->
    let SynBinding(headPat = pat) = binding
    match pat with
    | SynPat.LongIdent(longDotId, _, _, args, _, _) ->
      match longDotId with
      | SynLongIdent(id = [ id ]) when id.idText = "new" ->
        MemberCategory.Constructor
      | _ ->
        match args with
        | SynArgPats.Pats [] -> MemberCategory.Property
        | _ -> MemberCategory.Method
    | _ -> MemberCategory.Method
  | SynMemberDefn.GetSetMember _ -> MemberCategory.Property
  | SynMemberDefn.AutoProperty _ -> MemberCategory.Property
  | SynMemberDefn.AbstractSlot _ -> MemberCategory.Method
  | SynMemberDefn.ValField _ -> MemberCategory.Field
  | SynMemberDefn.LetBindings _ -> MemberCategory.Field
  | SynMemberDefn.NestedType _ -> MemberCategory.NestedType
  | _ -> MemberCategory.Method

let private isStaticMember (memberDefn: SynMemberDefn) =
  match memberDefn with
  | SynMemberDefn.Member(SynBinding(trivia = trivia), _) ->
    trivia.LeadingKeyword.IsStaticMember
  | _ -> false

let private getMemberScope (memberDefn: SynMemberDefn) =
  if isStaticMember memberDefn then MemberScope.Static else MemberScope.Instance

let private getAccessLevel = function
  | SynMemberDefn.Member(SynBinding(accessibility = access), _) ->
    match access with
    | Some(SynAccess.Private _) -> AccessLevel.Private
    | Some(SynAccess.Internal _) -> AccessLevel.Internal
    | Some(SynAccess.Public _) -> AccessLevel.Public
    | None -> AccessLevel.Public
  | _ -> AccessLevel.Public

let private getMemberOrderKey (memberDefn: SynMemberDefn) =
  let category = int (getMemberCategory memberDefn)
  let scope = int (getMemberScope memberDefn)
  let access = int (getAccessLevel memberDefn)
  category * 100 + scope * 10 + access

/// Traverses elements to find the presence of a self identifier.
/// Recursively searches through the given application structure,
/// accumulating results as it matches the specified pattern identifier.
let rec private findSelfIdentifierInApp src patIdent acc = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = id))
    when not id.IsEmpty && id.Head.idText = patIdent -> true
  | SynExpr.For(ident = ident) when ident.idText = patIdent -> true
  | SynExpr.Ident(ident = ident) when ident.idText = patIdent -> true
  | SynExpr.LongIdentSet(expr = expr)
  | SynExpr.Typed(expr = expr)
  | SynExpr.Assert(expr = expr)
  | SynExpr.Paren(expr = expr)
  | SynExpr.Downcast(expr = expr)
  | SynExpr.Upcast(expr = expr)
  | SynExpr.DotGet(expr = expr)
  | SynExpr.YieldOrReturn(expr = expr)
  | SynExpr.YieldOrReturnFrom(expr = expr)
  | SynExpr.ComputationExpr(expr = expr)
  | SynExpr.ArrayOrListComputed(expr = expr) ->
    findSelfIdentifierInApp src patIdent acc expr
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
  | SynExpr.TryWith(tryExpr = expr; withCases = clauses)
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
  | SynExpr.NamedIndexedPropertySet(longDotId = SynLongIdent(id = id)
                                    expr1 = expr1; expr2 = expr2) ->
    not id.IsEmpty && id.Head.idText = patIdent
    || findSelfIdentifierInApp src patIdent acc expr1
    || findSelfIdentifierInApp src patIdent acc expr2
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
  | SynExpr.Record(recordFields = recordFields) ->
    recordFields
    |> List.exists (fun field ->
      let SynExprRecordField(expr = expr) = field
      if expr.IsSome then findSelfIdentifierInApp src patIdent acc expr.Value
      else acc
    )
  | _ -> acc

let checkMemberOrder src (members: SynMemberDefn list) =
  members
  |> List.filter (fun m ->
    match m with
    | SynMemberDefn.ImplicitCtor _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.Inherit _
    | SynMemberDefn.Interface _ -> false
    | _ -> true
  )
  |> List.pairwise
  |> List.iter (fun (prev, next) ->
    let prevKey = getMemberOrderKey prev
    let nextKey = getMemberOrderKey next
    if prevKey > nextKey then
      let prevCat = getMemberCategory prev
      let nextCat = getMemberCategory next
      let prevScope = getMemberScope prev
      let nextScope = getMemberScope next
      if prevCat <> nextCat then
        reportWarn src next.Range
          $"Move {nextCat} before {prevCat}"
      elif prevScope <> nextScope then
        reportWarn src next.Range
          "Move static member before instance members"
      else
        reportWarn src next.Range
          "Fix member order by access level"
    else
      ()
  )

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
          reportWarn src dotRange "Remove whitespace after '``'"
        else ()
        false
      else
        true
  else
    true

/// Check the spacing between identifiers and parentheses based on their casing.
/// If either identifier is in PascalCase, no space between the identifier and
/// the opening parenthesis. If in lower case, ensures exactly one space exists.
let checkMemberSpacing (src: ISourceText) longId extraId dotRanges args =
  match (longId: LongIdent) with
  | id :: _ when id.idText = "this" || id.idText = "_" ->
    match args with
    | SynPat.Wild(range = range) :: wild when wild.Length <> 0 ->
      reportWarn src (Range.unionRanges range (List.last wild).Range)
        "Use non-curried parameter style"
    | SynPat.Paren(range = range) :: paren when paren.Length <> 0 ->
      reportWarn src (Range.unionRanges range (List.last paren).Range)
        "Use non-curried parameter style"
    | SynPat.Named(range = range) :: named when named.Length <> 0 ->
      reportWarn src (Range.unionRanges range (List.last named).Range)
        "Use non-curried parameter style"
    | [ SynPat.Paren(range = range) ] ->
      let lastId = List.last longId
      if checkBackticMethodSpacing src dotRanges range then
        if (extraId: Ident Option).IsSome
          && isPascalCase extraId.Value.idText
          && extraId.Value.idRange.EndColumn <> range.StartColumn
        then
          Range.mkRange "" extraId.Value.idRange.End range.Start
          |> reportPascalCaseError src
        elif (extraId: Ident Option).IsSome
          && extraId.Value.idRange.EndColumn <> range.StartColumn
        then
          Range.mkRange "" extraId.Value.idRange.End range.Start
          |> reportPascalCaseError src
        elif isPascalCase lastId.idText
          && lastId.idRange.EndColumn <> range.StartColumn
        then
          Range.mkRange "" lastId.idRange.End range.Start
          |> reportPascalCaseError src
        elif isPascalCase lastId.idText |> not
          && lastId.idRange.EndColumn + 1 <> range.StartColumn
        then
          Range.mkRange "" lastId.idRange.End range.Start
          |> reportLowerCaseError src
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
    | SynPat.Wild(range = range) :: wild when wild.Length <> 0 ->
      reportWarn src (Range.unionRanges range (List.last wild).Range)
        "Use non-curried parameter style"
    | SynPat.Paren(range = range) :: paren when paren.Length <> 0 ->
      reportWarn src (Range.unionRanges range (List.last paren).Range)
        "Use non-curried parameter style"
    | SynPat.Named(range = range) :: named when named.Length <> 0 ->
      reportWarn src (Range.unionRanges range (List.last named).Range)
        "Use non-curried parameter style"
    | [ SynPat.Paren(range = range) ] ->
      let idRange =
        match typarDecls with
        | Some(SynValTyparDecls(typars = Some typars)) -> typars.Range
        | _ -> id.idRange
      if idRange.EndColumn <> range.StartColumn then
        Range.mkRange "" idRange.End range.Start
        |> reportPascalCaseError src
      else ()
    | _ -> ()
  | _ when (idTrivia: list<option<IdentTrivia>>).Head.IsSome ->
    match idTrivia.Head.Value with
    | IdentTrivia.OriginalNotationWithParen(rightParenRange = range) ->
      match args with
      | [ SynPat.Paren(range = argRange) ] ->
        if range.EndColumn + 1 <> argRange.StartColumn then
          let wRange = Range.mkRange "" range.End argRange.Start
          reportWarn src wRange "Add single whitespace between Infix and '('"
        else ()
      | _ -> ()
    | _ -> warn $"[checkStaticMemberSpacing]TODO: {longId}"
  | _ -> ()

let checkSelfIdentifierUsage (src: ISourceText) pat body =
  match pat with
  | SynPat.LongIdent(longDotId = SynLongIdent(id = id))
    when not id.IsEmpty && id.Head.idText = "__" ->
      reportWarn src id.Head.idRange "Change '__' to 'this'"
  | SynPat.LongIdent(longDotId = SynLongIdent(id = id))
    when not id.IsEmpty
      && (id.Head.idText = "this" || id.Head.idText = "self") ->
    if findSelfIdentifierInApp src id.Head.idText false body then ()
    else
      [ id.Head.idRange.StartLine .. src.GetLineCount() - 1 ]
      |> List.map src.GetLineString
      |> List.takeWhile (fun line ->
        not (String.IsNullOrWhiteSpace(line.Trim())))
      |> List.exists (fun (line: string) -> line.Contains "this.")
      |> fun existCompFlag ->
        if not existCompFlag then
          reportWarn src id.Head.idRange "Remove unused self-identifier"
        else
          ()
  | _ -> ()