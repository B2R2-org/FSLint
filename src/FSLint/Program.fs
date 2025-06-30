module B2R2.FSLint.Program

open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let parseFile txt (path: string) =
  let checker = FSharpChecker.Create ()
  let src = SourceText.ofString txt
  let projOptions, _ =
    checker.GetProjectOptionsFromScript (path, src)
    |> Async.RunSynchronously
  let parsingOptions, _ =
    checker.GetParsingOptionsFromProjectOptions projOptions
  checker.ParseFile (path, src, parsingOptions)
  |> Async.RunSynchronously
  |> fun r -> src, r.ParseTree

let rec checkPattern src case isArg = function
  | SynPat.Attrib _
  | SynPat.Const _
  | SynPat.Record _
  | SynPat.Wild _ ->
    () (* no need to check this *)
  | SynPat.Named (ident = SynIdent (ident = id); range = range) ->
    IdentifierConvention.check src case true id.idText range
  | SynPat.Typed (pat = pat; targetType = typ; range = range) ->
    checkPattern src case isArg pat
    TypeAnnotation.check src pat typ range
  | SynPat.ListCons (lhsPat = lhs; rhsPat = rhs) ->
    checkPattern src case isArg lhs
    checkPattern src case isArg rhs
  | SynPat.LongIdent (lid, _, _, SynArgPats.Pats args, _, range) ->
    let SynLongIdent (id = lid) = lid
    let name = (List.last lid).idText
    let case = if not (List.isEmpty args) && isArg then PascalCase else case
    IdentifierConvention.check src case true name range
    for arg in args do checkPattern src LowerCamelCase true arg
  | SynPat.LongIdent (lid, _, _, SynArgPats.NamePatPairs _, _, range) ->
    let SynLongIdent (id = lid) = lid
    let name = (List.last lid).idText
    IdentifierConvention.check src PascalCase true name range
  | SynPat.Paren (pat = pat) ->
    checkPattern src case isArg pat
  | SynPat.Tuple (elementPats = pats) ->
    for pat in pats do checkPattern src case isArg pat
  | SynPat.OptionalVal (ident = id) ->
    IdentifierConvention.check src LowerCamelCase true id.idText id.idRange
  | pat ->
    failwith $"{nameof checkPattern} TODO: {pat}"

and checkSimplePattern src case = function
  | SynSimplePat.Id (ident = id) ->
    IdentifierConvention.check src case true id.idText id.idRange
  | SynSimplePat.Typed (pat = pat) ->
    checkSimplePattern src case pat
  | pat ->
    failwith $"{nameof checkSimplePattern} TODO: {pat}"

and checkMatchClause (src: ISourceText) clause =
  let SynMatchClause (pat = pat; resultExpr = expr) = clause
  ArrayOrListConvention.checkPattern src pat
  checkExpression src expr

and checkExpression src = function
  | SynExpr.Paren (expr = expr) ->
    checkExpression src expr
  | SynExpr.Typed (expr = expr) ->
    checkExpression src expr
  | SynExpr.Lambda (args = args; body = body) ->
    let SynSimplePats.SimplePats (pats = pats) = args
    for pat in pats do checkSimplePattern src LowerCamelCase pat
    checkExpression src body
  | SynExpr.LetOrUse (_, _, bindings, body, _, _) ->
    checkBindings src LowerCamelCase bindings
    checkExpression src body
  | SynExpr.Do (expr = expr)
  | SynExpr.For (doBody = expr)
  | SynExpr.ForEach (bodyExpr = expr)
  | SynExpr.While (doExpr = expr) ->
    checkExpression src expr
  | SynExpr.IfThenElse (ifExpr = ifExpr
                        thenExpr = thenExpr
                        elseExpr = elseExpr) ->
    checkExpression src ifExpr
    checkExpression src thenExpr
    if Option.isSome elseExpr then
      checkExpression src (Option.get elseExpr)
    else ()
  | SynExpr.Match (expr = expr; clauses = clauses) ->
    checkExpression src expr
    for clause in clauses do checkMatchClause src clause
  | SynExpr.MatchLambda (matchClauses = clauses) ->
    for clause in clauses do checkMatchClause src clause
  | SynExpr.Tuple (exprs = exprs) ->
    for expr in exprs do checkExpression src expr
  | SynExpr.TryFinally (tryExpr = tryExpr; finallyExpr = finallyExpr) ->
    checkExpression src tryExpr
    checkExpression src finallyExpr
  | SynExpr.TryWith (tryExpr = tryExpr; withCases = clauses) ->
    checkExpression src tryExpr
    for clause in clauses do checkMatchClause src clause
  | SynExpr.ArrayOrListComputed (isArray, expr, range) ->
    ArrayOrListConvention.check src isArray range expr
    checkExpression src expr
  | SynExpr.ArrayOrList (isArray, exprs, range) ->
    let enclosureWidth = if isArray then 4 else 2
    ArrayOrListConvention.checkEmpty src enclosureWidth exprs range
    for expr in exprs do checkExpression src expr
  | SynExpr.App (flag = flag; funcExpr = funcExpr; argExpr = argExpr) as expr ->
    match funcExpr, flag, argExpr.IsArrayOrListComputed with
    | _, ExprAtomicFlag.Atomic, true
    | SynExpr.Paren _, _, true
    | SynExpr.App (flag = ExprAtomicFlag.Atomic), _, true ->
      ArrayOrListConvention.checkSpacingInIndexedProperty src expr
    | _ ->
      checkExpression src funcExpr
      checkExpression src argExpr
  | SynExpr.Sequential (expr1 = expr1; expr2 = expr2) ->
    checkExpression src expr1
    checkExpression src expr2
  | SynExpr.DotSet (targetExpr = targetExpr; rhsExpr = rhsExpr) ->
    checkExpression src targetExpr
    checkExpression src rhsExpr
  | SynExpr.DotGet (expr = expr) ->
    checkExpression src expr
  | SynExpr.AddressOf _
  | SynExpr.Assert _
  | SynExpr.ComputationExpr _
  | SynExpr.Const _
  | SynExpr.DotIndexedGet _
  | SynExpr.DotIndexedSet _
  | SynExpr.DotNamedIndexedPropertySet _
  | SynExpr.Downcast _
  | SynExpr.Fixed _
  | SynExpr.Ident _
  | SynExpr.IndexRange _
  | SynExpr.InterpolatedString _
  | SynExpr.Lazy _
  | SynExpr.LongIdent _
  | SynExpr.LongIdentSet _
  | SynExpr.NamedIndexedPropertySet _
  | SynExpr.New _
  | SynExpr.Null _
  | SynExpr.ObjExpr _
  | SynExpr.Record _
  | SynExpr.Set _
  | SynExpr.TypeApp _
  | SynExpr.Upcast _
  | SynExpr.YieldOrReturn _
  | SynExpr.YieldOrReturnFrom _ ->
    () (* no need to check this *)
  | expr ->
    failwith $"{nameof checkExpression} TODO: {expr}"

and checkIdOpt src case = function
  | Some (id: Ident) ->
    IdentifierConvention.check src case true id.idText id.idRange
  | None -> failwith "?"

and checkMemberDefns src members =
  for memberDefn in members do
    match memberDefn with
    | SynMemberDefn.Member (binding, _) ->
      checkBinding src PascalCase binding
    | SynMemberDefn.GetSetMember (get, set, _, _) ->
      if get.IsSome then checkBinding src PascalCase get.Value else ()
      if set.IsSome then checkBinding src PascalCase set.Value else ()
    | SynMemberDefn.LetBindings (bindings = bindings) ->
      checkBindings src LowerCamelCase bindings
    | SynMemberDefn.AbstractSlot (slotSig = SynValSig (ident = id)) ->
      let SynIdent (ident = id) = id
      IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynMemberDefn.Interface (members = Some members) ->
      checkMemberDefns src members
    | SynMemberDefn.ValField (SynField (idOpt = idOpt), _) ->
      checkIdOpt src PascalCase idOpt
    | SynMemberDefn.AutoProperty (ident = id) ->
      IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynMemberDefn.ImplicitCtor _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.Inherit _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkMemberDefns} TODO: {memberDefn}"

and checkTypeDefnSimpleRepr src = function
  | SynTypeDefnSimpleRepr.Union (unionCases=cases) ->
    for case in cases do
      let SynUnionCase (ident = SynIdent (ident = id); range = range) = case
      IdentifierConvention.check src PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Enum (cases = cases) ->
    for case in cases do
      let SynEnumCase (ident = SynIdent (ident = id); range = range) = case
      IdentifierConvention.check src PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Record (recordFields = fields) ->
    for field in fields do
      let SynField (idOpt = idOpt) = field
      checkIdOpt src PascalCase idOpt
  | SynTypeDefnSimpleRepr.Exception repr ->
    checkExceptionDefnRepr src repr
  | SynTypeDefnSimpleRepr.TypeAbbrev _
  | SynTypeDefnSimpleRepr.None _ ->
    () (* no need to check this *)
  | repr ->
    failwith $"{nameof checkTypeDefnSimpleRepr} TODO: {repr}"

and checkExceptionDefnRepr src repr =
  let SynExceptionDefnRepr (caseName = caseName) = repr
  let SynUnionCase (ident = SynIdent (ident = id); range = range) = caseName
  IdentifierConvention.check src PascalCase true id.idText range

and checkTypeDefnRepr src repr =
  match repr with
  | SynTypeDefnRepr.ObjectModel (_, members, _) ->
    checkMemberDefns src members
  | SynTypeDefnRepr.Simple (repr, _) ->
    checkTypeDefnSimpleRepr src repr
  | SynTypeDefnRepr.Exception repr ->
    checkExceptionDefnRepr src repr

and checkTypeDefn src defn =
  let SynTypeDefn (typeInfo = info; typeRepr = repr; members = members) = defn
  let SynComponentInfo (longId = lid; range = range; attributes = attrs) = info
  let case = if hasAttr "Measure" attrs then LowerCamelCase else PascalCase
  let name = (List.last lid).idText
  IdentifierConvention.check src case true name range
  checkTypeDefnRepr src repr
  checkMemberDefns src members

and hasAttr attrName attrs =
  attrs
  |> List.exists (fun (lst: SynAttributeList) ->
    lst.Attributes
    |> List.exists (fun attr ->
      let SynLongIdent (id = lid) = attr.TypeName
      lid |> List.exists (fun id -> id.idText = attrName)
    )
  )

and checkBinding src case binding =
  let SynBinding (headPat = pat; expr = body; attributes = attrs) = binding
  let case = if hasAttr "Literal" attrs then PascalCase else case
  checkPattern src case false pat
  checkExpression src body

and checkBindings src case bindings =
  for binding in bindings do
    checkBinding src case binding

and checkDeclarations src decls =
  for decl in decls do
    match decl with
    | SynModuleDecl.ModuleAbbrev (ident = id) ->
      IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynModuleDecl.NestedModule (moduleInfo = info) ->
      let SynComponentInfo (longId = lid) = info
      for id in lid do
        IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynModuleDecl.Let (_, bindings, _range) ->
      checkBindings src LowerCamelCase bindings
    | SynModuleDecl.Expr (expr = expr) ->
      checkExpression src expr
    | SynModuleDecl.Types (typeDefns, _range) ->
      for typeDefn in typeDefns do checkTypeDefn src typeDefn
    | SynModuleDecl.Exception (SynExceptionDefn (exnRepr = repr), _) ->
      checkExceptionDefnRepr src repr
    | SynModuleDecl.Open _
    | SynModuleDecl.HashDirective _
    | SynModuleDecl.Attributes _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkDeclarations} TODO: {decl}"

let checkWithAST src = function
  | ParsedInput.ImplFile (ParsedImplFileInput (contents=modules))->
    for m in modules do
      let SynModuleOrNamespace (longId = lid; decls = decls) = m
      for id in lid do
        IdentifierConvention.check src PascalCase true id.idText id.idRange
      checkDeclarations src decls
  | ParsedInput.SigFile _ ->
    () (* ignore fsi files *)

let ensureNoBOM (bs: byte[]) =
  if bs.Length > 3 && bs[0] = 0xEFuy && bs[1] = 0xBBuy && bs[2] = 0xBFuy then
    exitWithError "Byte Order Mark (BOM) should be removed from the file."
  else
    bs

let lintFile (linter: ILintable) (path: string) =
  if not <| File.Exists path then exitWithError $"File '{path}' not found"
  else printfn $"Linting file: {path}"
  let bytes = File.ReadAllBytes path |> ensureNoBOM
  let txt = System.Text.Encoding.UTF8.GetString bytes
  linter.Lint path txt

let runLinter linter (path: string) =
  try lintFile linter path
  with LintException msg ->
    System.Console.WriteLine msg
    exit 1

let linterForFs =
  { new ILintable with
      member _.Lint path txt =
        LineConvention.check txt
        parseFile txt path ||> checkWithAST }

let linterForProjSln =
  { new ILintable with
      member _.Lint _path txt =
        LineConvention.checkWindowsLineEndings txt }

[<EntryPoint>]
let main args =
  if args.Length < 1 then exitWithError "Usage: fslint <file|dir>"
  elif File.Exists args[0] then
    runLinter linterForFs args[0]
    0
  elif Directory.Exists args[0] then
    runOnEveryProjectSlnFile args[0] (runLinter linterForProjSln)
    runOnEveryFsFile args[0] (runLinter linterForFs)
    System.Console.WriteLine "Linting completed."
    0
  else exitWithError $"File or directory '{args[0]}' not found"
