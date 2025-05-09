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
  |> fun r -> r.ParseTree

let rec checkPattern case isArg = function
  | SynPat.Attrib _
  | SynPat.Const _
  | SynPat.Record _
  | SynPat.Wild _ ->
    () (* no need to check this *)
  | SynPat.Named (ident = SynIdent (ident = id); range = range) ->
    IdentifierConvention.check case true id.idText range
  | SynPat.Typed (pat = pat) ->
    checkPattern case isArg pat
  | SynPat.LongIdent (lid, _, _, SynArgPats.Pats args, _, range) ->
    let SynLongIdent (id = lid) = lid
    let name = (List.last lid).idText
    let case = if not (List.isEmpty args) && isArg then PascalCase else case
    IdentifierConvention.check case true name range
    for arg in args do checkPattern LowerCamelCase true arg
  | SynPat.LongIdent (lid, _, _, SynArgPats.NamePatPairs _, _, range) ->
    let SynLongIdent (id = lid) = lid
    let name = (List.last lid).idText
    IdentifierConvention.check PascalCase true name range
  | SynPat.Paren (pat = pat) ->
    checkPattern case isArg pat
  | SynPat.Tuple (elementPats = pats) ->
    for pat in pats do checkPattern case isArg pat
  | SynPat.OptionalVal (ident = id) ->
    IdentifierConvention.check LowerCamelCase true id.idText id.idRange
  | pat ->
    failwith $"{nameof checkPattern} TODO: {pat}"

and checkSimplePattern case = function
  | SynSimplePat.Id (ident = id) ->
    IdentifierConvention.check case true id.idText id.idRange
  | SynSimplePat.Typed (pat = pat) ->
    checkSimplePattern case pat
  | pat ->
    failwith $"{nameof checkSimplePattern} TODO: {pat}"

and checkMatchClause clause =
  let SynMatchClause (resultExpr = expr) = clause
  checkExpression expr

and checkExpression = function
  | SynExpr.Paren (expr = expr) ->
    checkExpression expr
  | SynExpr.Typed (expr = expr) ->
    checkExpression expr
  | SynExpr.Lambda (args = args; body = body) ->
    let SynSimplePats.SimplePats (pats = pats) = args
    for pat in pats do checkSimplePattern LowerCamelCase pat
    checkExpression body
  | SynExpr.LetOrUse (_, _, bindings, body, _, _) ->
    checkBindings LowerCamelCase bindings
    checkExpression body
  | SynExpr.Do (expr = expr)
  | SynExpr.For (doBody = expr)
  | SynExpr.ForEach (bodyExpr = expr)
  | SynExpr.While (doExpr = expr) ->
    checkExpression expr
  | SynExpr.IfThenElse (thenExpr = thenExpr; elseExpr = elseExpr) ->
    checkExpression thenExpr
    if Option.isSome elseExpr then checkExpression (Option.get elseExpr)
  | SynExpr.MatchLambda (matchClauses = clauses)
  | SynExpr.Match (clauses = clauses) ->
    for clause in clauses do checkMatchClause clause
  | SynExpr.Tuple (exprs = exprs) ->
    for expr in exprs do checkExpression expr
  | SynExpr.TryFinally (tryExpr = tryExpr; finallyExpr = finallyExpr) ->
    checkExpression tryExpr
    checkExpression finallyExpr
  | SynExpr.TryWith (tryExpr = tryExpr; withCases = clauses) ->
    checkExpression tryExpr
    for clause in clauses do checkMatchClause clause
  | SynExpr.ArrayOrList _
  | SynExpr.ArrayOrListComputed _
  | SynExpr.App _
  | SynExpr.Assert _
  | SynExpr.Const _
  | SynExpr.DotGet _
  | SynExpr.DotIndexedGet _
  | SynExpr.DotIndexedSet _
  | SynExpr.DotNamedIndexedPropertySet _
  | SynExpr.DotSet _
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
  | SynExpr.Sequential _
  | SynExpr.Set _
  | SynExpr.TypeApp _
  | SynExpr.Upcast _ ->
    () (* no need to check this *)
  | expr ->
    failwith $"{nameof checkExpression} TODO: {expr}"

and checkIdOpt case = function
  | Some (id: Ident) ->
    IdentifierConvention.check case true id.idText id.idRange
  | None -> failwith "?"

and checkMemberDefns members =
  for memberDefn in members do
    match memberDefn with
    | SynMemberDefn.Member (binding, _) ->
      checkBinding PascalCase binding
    | SynMemberDefn.GetSetMember (get, set, _, _) ->
      if get.IsSome then checkBinding PascalCase get.Value else ()
      if set.IsSome then checkBinding PascalCase set.Value else ()
    | SynMemberDefn.LetBindings (bindings = bindings) ->
      checkBindings LowerCamelCase bindings
    | SynMemberDefn.AbstractSlot (slotSig = SynValSig (ident = id)) ->
      let SynIdent (ident = id) = id
      IdentifierConvention.check PascalCase true id.idText id.idRange
    | SynMemberDefn.Interface (members = Some members) ->
      checkMemberDefns members
    | SynMemberDefn.ValField (SynField (idOpt = idOpt), _) ->
      checkIdOpt PascalCase idOpt
    | SynMemberDefn.AutoProperty (ident = id) ->
      IdentifierConvention.check PascalCase true id.idText id.idRange
    | SynMemberDefn.ImplicitCtor _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.Inherit _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkMemberDefns} TODO: {memberDefn}"

and checkTypeDefnSimpleRepr = function
  | SynTypeDefnSimpleRepr.Union (unionCases=cases) ->
    for case in cases do
      let SynUnionCase (ident = SynIdent (ident = id); range = range) = case
      IdentifierConvention.check PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Enum (cases = cases) ->
    for case in cases do
      let SynEnumCase (ident = SynIdent (ident = id); range = range) = case
      IdentifierConvention.check PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Record (recordFields = fields) ->
    for field in fields do
      let SynField (idOpt = idOpt) = field
      checkIdOpt PascalCase idOpt
  | SynTypeDefnSimpleRepr.Exception repr ->
    checkExceptionDefnRepr repr
  | SynTypeDefnSimpleRepr.TypeAbbrev _
  | SynTypeDefnSimpleRepr.None _ ->
    () (* no need to check this *)
  | repr ->
    failwith $"{nameof checkTypeDefnSimpleRepr} TODO: {repr}"

and checkExceptionDefnRepr repr =
  let SynExceptionDefnRepr (caseName = caseName) = repr
  let SynUnionCase (ident = SynIdent (ident = id); range = range) = caseName
  IdentifierConvention.check PascalCase true id.idText range

and checkTypeDefnRepr repr =
  match repr with
  | SynTypeDefnRepr.ObjectModel (_, members, _) ->
    checkMemberDefns members
  | SynTypeDefnRepr.Simple (repr, _) ->
    checkTypeDefnSimpleRepr repr
  | SynTypeDefnRepr.Exception repr ->
    checkExceptionDefnRepr repr

and checkTypeDefn defn =
  let SynTypeDefn (typeInfo = info; typeRepr = repr; members = members) = defn
  let SynComponentInfo (longId = lid; range = range; attributes = attrs) = info
  let case = if hasAttr "Measure" attrs then LowerCamelCase else PascalCase
  let name = (List.last lid).idText
  IdentifierConvention.check case true name range
  checkTypeDefnRepr repr
  checkMemberDefns members

and checkDeclarations decls =
  for decl in decls do
    match decl with
    | SynModuleDecl.ModuleAbbrev (ident = id) ->
      IdentifierConvention.check PascalCase true id.idText id.idRange
    | SynModuleDecl.NestedModule (moduleInfo = info) ->
      let SynComponentInfo (longId = lid) = info
      for id in lid do
        IdentifierConvention.check PascalCase true id.idText id.idRange
    | SynModuleDecl.Let (_, bindings, _range) ->
      checkBindings LowerCamelCase bindings
    | SynModuleDecl.Expr (expr = expr) ->
      checkExpression expr
    | SynModuleDecl.Types (typeDefns, _range) ->
      for typeDefn in typeDefns do checkTypeDefn typeDefn
    | SynModuleDecl.Exception (SynExceptionDefn (exnRepr = repr), _) ->
      checkExceptionDefnRepr repr
    | SynModuleDecl.Open _
    | SynModuleDecl.HashDirective _
    | SynModuleDecl.Attributes _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkDeclarations} TODO: {decl}"

and hasAttr attrName attrs =
  attrs
  |> List.exists (fun (lst: SynAttributeList) ->
    lst.Attributes
    |> List.exists (fun attr ->
      let SynLongIdent (id = lid) = attr.TypeName
      lid |> List.exists (fun id -> id.idText = attrName)
    )
  )

and checkBinding case binding =
  let SynBinding (headPat = pat; expr = body; attributes = attrs) = binding
  let case = if hasAttr "Literal" attrs then PascalCase else case
  checkPattern case false pat
  checkExpression body

and checkBindings case bindings =
  for binding in bindings do
    checkBinding case binding

let checkWithString txt =
  LineConvention.check txt

let checkWithAST txt path =
  parseFile txt path
  |> function
    | ParsedInput.ImplFile (ParsedImplFileInput (contents=modules))->
      for m in modules do
        let SynModuleOrNamespace (longId = lid; decls = decls) = m
        for id in lid do
          IdentifierConvention.check PascalCase true id.idText id.idRange
        checkDeclarations decls
      printfn "Linting completed."
    | ParsedInput.SigFile _ ->
      () (* ignore fsi files *)

let lintTextString (path: string) (txt: string) =
  checkWithString txt
  checkWithAST txt path

let lintFile (path: string) =
  if not <| File.Exists path then exitWithError $"File '{path}' not found"
  else printfn $"Linting file: {path}"
  let txt = File.ReadAllText path
  lintTextString path txt

let lintFileAndExitWhenFailed (path: string) =
  try lintFile path
  with LintException msg ->
    System.Console.WriteLine msg
    exit 1

[<EntryPoint>]
let main args =
  if args.Length < 1 then exitWithError "Usage: fslint <file|dir>"
  elif File.Exists args[0] then lintFileAndExitWhenFailed args[0]; 0
  elif Directory.Exists args[0] then
    runOnEveryFile args[0] lintFileAndExitWhenFailed; 0
  else exitWithError $"File or directory '{args[0]}' not found"
