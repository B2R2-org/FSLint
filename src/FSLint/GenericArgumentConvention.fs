module B2R2.FSLint.GenericArgumentConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let collectRangeOfFirstAndLastType (typeArgs: SynType list) =
  typeArgs
  |> List.map (fun synType -> synType.Range)
  |> List.reduce Range.unionRanges

let collectTypeArgsRange typeArgs =
  typeArgs
  |> List.fold (fun acc synType ->
    match synType with
    | SynType.Tuple(path = path) ->
      path
      |> List.collect (fun innerPath ->
        if innerPath.IsStar || innerPath.IsSlash then
          (false, innerPath.Range) :: acc
        else (true, innerPath.Range) :: acc
      )
    | synType -> (true, synType.Range) :: acc
  ) []
  |> List.sortBy (fun (_, range) -> range.StartColumn)

/// Checks that no whitespace in null type arguments.
let checkEmpty src (typeArgsRange: range) =
  if typeArgsRange.EndColumn - typeArgsRange.StartColumn <> 3 then
    reportError src typeArgsRange "Contains invalid whitespace"
  else ()

/// Checks if there is a space between 'expr' and '<type>' in an expression,
/// e.g., detects 'expr <type>' instead of the correct 'expr<type>'.
let checkFromExprToOpeningBracketSpacing src expr typeArgsRange =
  if (expr: SynExpr).Range.EndColumn <> (typeArgsRange: range).StartColumn then
    reportError src typeArgsRange "Contains invalid whitespace"
  else ()

/// Checks if there is a space between '<' and 'type' or 'type' and '>' in the
/// given type argument range.
let checkBracketSpacing src typeArgsRange edgeRange =
  if (edgeRange: range).StartColumn - 1 <> (typeArgsRange: range).StartColumn
    || edgeRange.EndColumn + 1 <> typeArgsRange.EndColumn
  then reportError src edgeRange "Contains invalid whitespace"
  else ()

/// Checks whether the spacing between elements in the given type argument list
/// is formatted correctly, distinguishing between ',' and '*' separators.
let checkTypeElementSpacing (src: ISourceText) (typeArgs: SynType list) =
  typeArgs
  |> List.map (fun typeArg -> typeArg.Range)
  |> List.iter (fun typeRange ->
    let typeStr = src.GetSubTextFromRange typeRange
    if typeStr.Contains '*' then
      let star = typeStr.Split '*'
      if (Array.head star).EndsWith " " && (Array.last star).StartsWith " " then
        ()
      else
        reportError src typeRange "Need single space between type."
    else
      let typeStr = src.GetLineString(typeRange.StartLine - 1)
      if typeStr.IndexOf ',' <> -1
        && typeStr.IndexOf ',' + 1 <> typeStr.Length then
        let comma = typeStr.Split ','
        if (Array.last comma)[0] = ' ' && (Array.last comma)[1] <> ' ' then ()
        else reportError src typeRange "Need single space between type."
        if (Array.head comma).EndsWith " " then
          reportError src typeRange "No space allowed before comma."
        else
          ()
      else ()
  )

/// Checks if there is an unwanted space before parentheses in type application,
/// Ensures proper spacing in type application expressions.
let checkTypeAppParenSpacing src = function
  | SynExpr.App(flag = flag
                funcExpr = funcExpr
                argExpr = SynExpr.Paren(leftParenRange = parenRange)) ->
    match funcExpr with
    | SynExpr.TypeApp(expr = SynExpr.LongIdent(longDotId = longDotId)
                      range = typeRange) ->
      let SynLongIdent(id = id) = longDotId
      if id.Length <> 1 then
        if flag = ExprAtomicFlag.NonAtomic
          || typeRange.EndColumn <> parenRange.StartColumn then
          reportError src parenRange "Contains invalid whitespace"
        else ()
      else () (* This handle Type Reference convention *)
    | _ -> ()
  | _ -> ()

let checkTypeAbbrev src (rhsType: SynType) =
  match rhsType with
  | SynType.App(lessRange = lessRange
                typeArgs = typeArgs
                greaterRange = greaterRange) ->
    let innerRange = collectRangeOfFirstAndLastType typeArgs
    match lessRange, greaterRange with
    | Some lessRange, None ->
      if innerRange.StartColumn <> lessRange.EndColumn then
        reportError src lessRange "Contains invalid whitespace"
      else ()
    | None, Some greaterRange ->
      if innerRange.EndColumn <> greaterRange.StartColumn then
        reportError src greaterRange "Contains invalid whitespace"
      else ()
    | Some lessRange, Some greaterRange ->
      let range = Range.unionRanges lessRange greaterRange
      checkBracketSpacing src range innerRange
    | _ -> ()
    typeArgs
    |> List.iter (fun typeArg ->
      match typeArg with
      | SynType.LongIdent(SynLongIdent(id = id)) ->
        if id.Length < 2 then
          ()
        else
          id
          |> List.map (_.idRange)
          |> List.pairwise
          |> List.iter (fun (front, back) ->
            if front.EndColumn + 1 <> back.StartColumn then
              reportError src back "Contains invalid whitespace"
            else ()
          )
      | _ -> ()
    )
    checkTypeElementSpacing src typeArgs
  | _ -> ()

let check src expr (typeArgs: SynType list) typeArgsRange =
  checkFromExprToOpeningBracketSpacing src expr typeArgsRange
  match List.tryHead typeArgs with
  | Some _ ->
    collectRangeOfFirstAndLastType typeArgs
    |> checkBracketSpacing src typeArgsRange
    checkTypeElementSpacing src typeArgs
  | None -> checkEmpty src typeArgsRange