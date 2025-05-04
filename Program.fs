module B2R2.FSLint.Program

open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let parseFile (path: string) =
  let checker = FSharpChecker.Create ()
  let src = File.ReadAllText path |> SourceText.ofString
  let projOptions, _ =
    checker.GetProjectOptionsFromScript (path, src)
    |> Async.RunSynchronously
  let parsingOptions, _ =
    checker.GetParsingOptionsFromProjectOptions projOptions
  checker.ParseFile (path, src, parsingOptions)
  |> Async.RunSynchronously
  |> fun r -> r.ParseTree

let rec checkPattern case = function
  | SynPat.Const _
  | SynPat.Wild _ ->
    () (* no need to check this *)
  | SynPat.Named (ident = SynIdent (ident = id); range = range) ->
    IdentifierConvention.check case id.idText range
  | SynPat.Typed (pat = pat) ->
    checkPattern case pat
  | SynPat.LongIdent (lid, _, _, SynArgPats.Pats args, _, range) ->
    let SynLongIdent (id = lid) = lid
    let name = (List.last lid).idText
    IdentifierConvention.check case name range
    for arg in args do checkPattern LowerCamelCase arg
  | SynPat.LongIdent (lid, _, _, SynArgPats.NamePatPairs _, _, range) ->
    let SynLongIdent (id = lid) = lid
    let name = (List.last lid).idText
    IdentifierConvention.check PascalCase name range
  | SynPat.Paren (pat = pat) ->
    checkPattern case pat
  | SynPat.Tuple (elementPats = pats) ->
    for pat in pats do checkPattern case pat
  | pat ->
    failwith $"{nameof checkPattern} TODO: {pat}"

and checkExpression = function
  | SynExpr.Paren (expr = expr) ->
    checkExpression expr
  | SynExpr.Typed (expr = expr) ->
    checkExpression expr
  | SynExpr.MatchLambda (matchClauses = clauses)
  | SynExpr.Match (clauses = clauses) ->
    let SynMatchClause (pat = lastPattern) = List.last clauses
    checkPattern LowerCamelCase lastPattern
  | SynExpr.LetOrUse (_, _, bindings, body, _, _) ->
    checkBindings LowerCamelCase bindings
    checkExpression body
  | SynExpr.Const _
  | SynExpr.ForEach _
  | SynExpr.Ident _
  | SynExpr.IfThenElse _
  | SynExpr.DotGet _
  | SynExpr.Sequential _
  | SynExpr.App _ ->
    () (* no need to check this *)
  | expr ->
    failwith $"{nameof checkExpression} TODO: {expr}"

and checkMemberDefns members =
  for memberDefn in members do
    match memberDefn with
    | SynMemberDefn.Member (binding, _) ->
      checkBinding PascalCase binding
    | SynMemberDefn.GetSetMember _ ->
      failwith "TODO"
    | SynMemberDefn.LetBindings (bindings = bindings) ->
      checkBindings LowerCamelCase bindings
    | SynMemberDefn.AbstractSlot (slotSig = SynValSig (ident = id)) ->
      let SynIdent (ident = id) = id
      IdentifierConvention.check PascalCase id.idText id.idRange
    | SynMemberDefn.Interface (members = Some members) ->
      checkMemberDefns members
    | SynMemberDefn.ImplicitCtor _
    | SynMemberDefn.Inherit _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkMemberDefns} TODO: {memberDefn}"

and checkTypeDefnSimpleRepr = function
  | SynTypeDefnSimpleRepr.Union (unionCases=cases) ->
    for case in cases do
      let SynUnionCase (ident = SynIdent (ident = id); range = range) = case
      IdentifierConvention.check PascalCase id.idText range
  | SynTypeDefnSimpleRepr.Enum (cases = cases) ->
    for case in cases do
      let SynEnumCase (ident = SynIdent (ident = id); range = range) = case
      IdentifierConvention.check PascalCase id.idText range
  | SynTypeDefnSimpleRepr.Record (recordFields = fields) ->
    for field in fields do
      let SynField (idOpt = idOpt; range = range) = field
      match idOpt with
      | Some id -> IdentifierConvention.check PascalCase id.idText range
      | None -> failwith "?"
  | SynTypeDefnSimpleRepr.Exception repr ->
    checkExceptionDefnRepr repr
  | _ ->
    failwith "TODO"

and checkExceptionDefnRepr repr =
  let SynExceptionDefnRepr (caseName = caseName) = repr
  let SynUnionCase (ident = SynIdent (ident = id); range = range) = caseName
  IdentifierConvention.check PascalCase id.idText range

and checkTypeDefn defn =
  let SynTypeDefn (typeInfo = info; typeRepr = repr; members = members) = defn
  let SynComponentInfo (longId = lid; range = range) = info
  let name = (List.last lid).idText
  IdentifierConvention.check PascalCase name range
  match repr with
  | SynTypeDefnRepr.ObjectModel (_, members, _) ->
    checkMemberDefns members
  | SynTypeDefnRepr.Simple (repr, _) ->
    checkTypeDefnSimpleRepr repr
  | SynTypeDefnRepr.Exception repr ->
    checkExceptionDefnRepr repr

and checkDeclarations decls =
  for decl in decls do
    match decl with
    | SynModuleDecl.ModuleAbbrev (ident = id) ->
      IdentifierConvention.check PascalCase id.idText id.idRange
    | SynModuleDecl.Let (_, bindings, _range) ->
      checkBindings LowerCamelCase bindings
    | SynModuleDecl.Expr (expr = expr) ->
      checkExpression expr
    | SynModuleDecl.Types (typeDefns, _range) ->
      for typeDefn in typeDefns do checkTypeDefn typeDefn
    | SynModuleDecl.Exception (SynExceptionDefn (exnRepr = repr), _) ->
      checkExceptionDefnRepr repr
    | SynModuleDecl.Open _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkDeclarations} TODO: {decl}"

and checkBinding case binding =
  let SynBinding (headPat = pat; expr = body) = binding
  checkPattern case pat
  checkExpression body

and checkBindings case bindings =
  for binding in bindings do
    checkBinding case binding

let lintFile (path: string) =
  if not <| File.Exists path then exitWithError $"File '{path}' not found"
  else printfn $"Linting file: {path}"
  parseFile path
  |> function
    | ParsedInput.ImplFile (ParsedImplFileInput (contents=modules))->
      for m in modules do
        let SynModuleOrNamespace (longId = lid; decls = decls) = m
        for id in lid do
          IdentifierConvention.check PascalCase id.idText id.idRange
        checkDeclarations decls
      printfn "Linting completed."
    | ParsedInput.SigFile _ ->
      () (* ignore fsi files *)

[<EntryPoint>]
let main args =
  if args.Length < 1 then exitWithError "Usage: FSLint <file.fs>"
  else lintFile args[0]; 0