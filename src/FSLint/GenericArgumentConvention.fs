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

let checkEmpty src (typeArgsRange: range) =
  if typeArgsRange.EndColumn - typeArgsRange.StartColumn <> 3 then
    reportError src typeArgsRange "Contains invalid whitespace"
  else ()

let checkFromExprToOpeningBracketSpacing src expr typeArgsRange =
  if (expr: SynExpr).Range.EndColumn <> (typeArgsRange: range).StartColumn then
    reportError src typeArgsRange "Contains invalid whitespace"
  else ()

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

let checkTypeAppParenSpacing src = function
  | SynExpr.App (flag = flag
                 funcExpr = SynExpr.TypeApp (range = typeRange)
                 argExpr = SynExpr.Paren (leftParenRange = leftParenRange)) ->
    if flag = ExprAtomicFlag.Atomic
      || typeRange.EndColumn + 1 <> leftParenRange.StartColumn then
      reportError src leftParenRange "TypeApp and Paren need a single space."
    else ()
  | _ -> ()

let check src expr (typeArgs: SynType list) typeArgsRange =
  checkFromExprToOpeningBracketSpacing src expr typeArgsRange
  match List.tryHead typeArgs with
  | Some _ ->
    collectRangeOfFirstAndLastType typeArgs
    |> checkBracketSpacing src typeArgsRange
    checkTypeElementSpacing src typeArgs
  | None -> checkEmpty src typeArgsRange