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
    | SynType.Tuple (path = path) ->
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

let subStringBetweenTypeArg (src: ISourceText) frontType (endType: range) =
  (Position.mkPos (frontType: range).StartLine frontType.EndColumn,
  Position.mkPos frontType.StartLine endType.StartColumn)
  ||> Range.mkRange ""
  |> src.GetSubTextFromRange

/// Checks whether the spacing between elements in the given type argument list
/// is formatted correctly, distinguishing between ',' and '*' separators.
let checkTypeElementSpacing src typeArgs =
  typeArgs
  |> collectTypeArgsRange
  |> List.pairwise
  |> List.iter (fun ((frontIsNormal, frontType: range),
    (endIsNormal, endType: range)) ->
    if frontIsNormal && endIsNormal then
      let subStr = subStringBetweenTypeArg src frontType endType
      if subStr.Length <> 2 then
        reportError src frontType "Need single space between type."
      elif subStr.EndsWith "," then
        reportError src frontType "No space allowed before comma"
      else ()
    else
      if endType.StartColumn - 1 <> frontType.EndColumn then
        reportError src frontType "Need single space between type."
      else ()
  )

/// Checks if there is an unwanted space before parentheses in type application,
/// Ensures proper spacing in type application expressions.
let checkTypeAppParenSpacing src = function
  | SynExpr.App (flag = flag
                 funcExpr = funcExpr
                 argExpr = SynExpr.Paren (leftParenRange = parenRange)) ->
    match funcExpr with
    | SynExpr.TypeApp (expr = SynExpr.LongIdent (longDotId = longDotId)
                       range = typeRange) ->
      let SynLongIdent (id = id) = longDotId
      if id.Length <> 1 then
        if flag = ExprAtomicFlag.NonAtomic
          || typeRange.EndColumn <> parenRange.StartColumn then
          reportError src parenRange "Contains invalid whitespace"
        else ()
      else () (* This handle Type Reference convention *)
    | _ -> ()
  | _ -> ()

let check src expr (typeArgs: SynType list) typeArgsRange =
  checkFromExprToOpeningBracketSpacing src expr typeArgsRange
  match List.tryHead typeArgs with
  | Some _ ->
    collectRangeOfFirstAndLastType typeArgs
    |> checkBracketSpacing src typeArgsRange
    checkTypeElementSpacing src typeArgs
  | None -> checkEmpty src typeArgsRange