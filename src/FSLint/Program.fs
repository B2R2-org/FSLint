module B2R2.FSLint.Program

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open type IdentifierConvention.CaseStyle

type CheckContext =
  { ModuleAccess: AccessModifierConvention.AccessLevel }

let defaultCheckContext =
  { ModuleAccess = AccessModifierConvention.AccessLevel.Public }

let parseFile txt (path: string) =
  let checker = FSharpChecker.Create()
  let src = SourceText.ofString txt
  let projOptions, _ =
    checker.GetProjectOptionsFromScript(path, src)
    |> Async.RunSynchronously
  let parsingOptions, _ =
    checker.GetParsingOptionsFromProjectOptions projOptions
  checker.ParseFile(path, src, parsingOptions)
  |> Async.RunSynchronously
  |> fun r -> src, r.ParseTree

let rec checkPattern src case isArg (trivia: SynBindingTrivia) = function
  | SynPat.Attrib _
  | SynPat.Const _
  | SynPat.Record _
  | SynPat.Wild _ ->
    () (* no need to check this *)
  | SynPat.Named(ident = SynIdent(ident = id); range = range) ->
    IdentifierConvention.check src case true id.idText range
  | SynPat.Typed(pat = pat; targetType = typ; range = range) ->
    checkPattern src case isArg trivia pat
    TypeAnnotation.checkPat src pat range typ
  | SynPat.ListCons(lhsPat = lhs; rhsPat = rhs) ->
    checkPattern src case isArg trivia lhs
    checkPattern src case isArg trivia rhs
  | SynPat.LongIdent(lid, extraId, typarDecls, SynArgPats.Pats args,
                     _, range) as pat ->
    let SynLongIdent(id = lid; dotRanges = dotRanges; trivia = idTrivia) = lid
    let name = (List.last lid).idText
    let case = if not (List.isEmpty args) && isArg then PascalCase else case
    IdentifierConvention.check src case true name range
    if trivia.LeadingKeyword.IsStaticMember then
      ClassMemberConvention.checkStaticMemberSpacing src lid typarDecls
        args idTrivia
    else
      ClassMemberConvention.checkMemberSpacing src lid extraId
        dotRanges args
    PatternMatchingConvention.checkBody src pat
    for arg in args do checkPattern src LowerCamelCase true trivia arg
  | SynPat.LongIdent(lid, _, _, SynArgPats.NamePatPairs(pats = pat), _, range)
    ->
    let SynLongIdent(id = lid) = lid
    let name = (List.last lid).idText
    IdentifierConvention.check src PascalCase true name range
    AssignmentConvention.checkNamePatPairs src pat
  | SynPat.Paren(pat = pat) as synPat ->
    ParenConvention.checkPat src synPat
    checkPattern src case isArg trivia pat
  | SynPat.Tuple(elementPats = pats) ->
    for pat in pats do checkPattern src case isArg trivia pat
  | SynPat.OptionalVal(ident = id) ->
    IdentifierConvention.check src LowerCamelCase true id.idText id.idRange
  | SynPat.As(lhsPat = lhsPat; rhsPat = rhsPat) ->
    checkPattern src case isArg trivia lhsPat
    checkPattern src case isArg trivia rhsPat
  | pat ->
    failwith $"{nameof checkPattern} TODO: {pat}"

and checkSimplePattern src case = function
  | SynSimplePat.Id(ident = id) ->
    IdentifierConvention.check src case true id.idText id.idRange
  | SynSimplePat.Typed(pat = pat; targetType = targetType) ->
    TypeAnnotation.checkFunction src pat targetType
    checkSimplePattern src case pat
  | pat ->
    failwith $"{nameof checkSimplePattern} TODO: {pat}"

and checkMatchClause (src: ISourceText) clause =
  let SynMatchClause(pat = pat; whenExpr = whenExpr; resultExpr = expr) = clause
  PatternMatchingConvention.checkBody src pat
  TypeUseConvention.checkParamTypeSpacing src pat
  match pat with
  | SynPat.LongIdent(argPats = SynArgPats.NamePatPairs(pats = pats)) ->
    FunctionCallConvention.checkMethodParenSpacing src expr
    AssignmentConvention.checkNamePatPairs src pats
  | SynPat.LongIdent(argPats = SynArgPats.Pats pats) ->
    PatternMatchingConvention.checkParenTupleSpacing src pats
  | _ -> ()
  if whenExpr.IsSome then
    FunctionCallConvention.checkMethodParenSpacing src whenExpr.Value
    checkExpression src whenExpr.Value
  else ()
  checkExpression src expr

and checkExpression src = function
  | SynExpr.Paren(expr = innerExpr) as expr ->
    ParenConvention.checkExpr src expr
    checkExpression src innerExpr
  | SynExpr.Typed(expr = expr) ->
    checkExpression src expr
  | SynExpr.Lambda(args = args; body = body) ->
    let SynSimplePats.SimplePats(pats = pats) = args
    for pat in pats do checkSimplePattern src LowerCamelCase pat
    checkExpression src body
  | SynExpr.LetOrUse(_, _, bindings, body, _, _) ->
    checkBindings src LowerCamelCase bindings
    checkExpression src body
  | SynExpr.LetOrUseBang(rhs = rhs; body = body) ->
    (* TODO: checkPattern for LetOrUseBang *)
    checkExpression src rhs
    checkExpression src body
  | SynExpr.ForEach(pat = pat; enumExpr = enumExpr; bodyExpr = bodyExpr) ->
    PatternMatchingConvention.checkBody src pat
    checkExpression src enumExpr
    checkExpression src bodyExpr
  | SynExpr.Do(expr = expr)
  | SynExpr.DoBang(expr = expr)
  | SynExpr.For(doBody = expr) ->
    checkExpression src expr
  | SynExpr.While(whileExpr = whileExpr; doExpr = doExpr) ->
    checkExpression src whileExpr
    checkExpression src doExpr
  | SynExpr.IfThenElse(ifExpr = ifExpr
                       thenExpr = thenExpr
                       elseExpr = elseExpr
                       range = range) ->
    IfThenElseConvention.check src ifExpr thenExpr elseExpr range
    checkExpression src ifExpr
    checkExpression src thenExpr
    if Option.isSome elseExpr then checkExpression src (Option.get elseExpr)
    else ()
  | SynExpr.MatchBang(expr = expr; clauses = clauses) ->
    checkExpression src expr
    PatternMatchingConvention.checkFormat src clauses
  | SynExpr.Match(expr = expr; clauses = clauses; trivia = trivia) ->
    checkExpression src expr
    PatternMatchingConvention.checkBarIsSameColWithMatch src clauses trivia
    PatternMatchingConvention.checkFormat src clauses
    for clause in clauses do checkMatchClause src clause
  | SynExpr.MatchLambda(matchClauses = clauses) ->
    for clause in clauses do checkMatchClause src clause
  | SynExpr.Tuple(exprs = exprs; commaRanges = commaRanges) ->
    TupleConvention.check src exprs commaRanges
    for expr in exprs do
      FunctionCallConvention.checkMethodParenSpacing src expr
      checkExpression src expr
  | SynExpr.TryFinally(tryExpr = tryExpr; finallyExpr = finallyExpr) ->
    checkExpression src tryExpr
    checkExpression src finallyExpr
  | SynExpr.TryWith(tryExpr = tryExpr; withCases = clauses) ->
    checkExpression src tryExpr
    TryWithConvention.check src clauses
    for clause in clauses do checkMatchClause src clause
  | SynExpr.ArrayOrListComputed(isArray, expr, range) ->
    ArrayOrListConvention.check src isArray range expr
    checkExpression src expr
  | SynExpr.ArrayOrList(isArray, exprs, range) ->
    let enclosureWidth = if isArray then 4 else 2
    ArrayOrListConvention.checkEmpty src enclosureWidth exprs range
    for expr in exprs do checkExpression src expr
  | SynExpr.App(flag = flag
                isInfix = isInfix
                funcExpr = funcExpr
                argExpr = argExpr) as expr ->
    NegationSimplificationConvention.check src expr
    match funcExpr, flag, argExpr.IsArrayOrListComputed with
    | _, ExprAtomicFlag.Atomic, true
    | SynExpr.Paren _, _, true
    | SynExpr.App(flag = ExprAtomicFlag.Atomic), _, true ->
      IndexedPropertyConvention.check src expr
    | _ ->
      TypeUseConvention.checkTypeAppParenSpacing src expr
      FunctionCallConvention.checkMethodParenSpacing src expr
      AppConvention.check src isInfix flag funcExpr argExpr
      AppConvention.checkUnaryOperatorSpacing src expr
      checkExpression src funcExpr
      checkExpression src argExpr
  | SynExpr.Sequential(expr1 = expr1; expr2 = expr2) ->
    checkExpression src expr1
    checkExpression src expr2
  | SynExpr.DotSet(targetExpr = targetExpr; rhsExpr = rhsExpr) ->
    checkExpression src targetExpr
    checkExpression src rhsExpr
  | SynExpr.DotGet(expr, dotm, longDotId, _) ->
    FunctionCallConvention.checkDotGetSpacing src expr dotm longDotId
    FunctionCallConvention.checkMethodParenSpacing src expr
    checkExpression src expr
  | SynExpr.YieldOrReturn(expr = expr)
  | SynExpr.YieldOrReturnFrom(expr = expr) ->
    checkExpression src expr
  | SynExpr.Upcast(expr = expr; targetType = targetType)
  | SynExpr.Downcast(expr = expr; targetType = targetType) ->
    TypeCastConvention.check src expr targetType
    checkExpression src expr
  | SynExpr.Const _ as expr ->
    ParenConvention.checkExpr src expr
  | SynExpr.TypeApp(expr = expr
                    typeArgs = typeArgs
                    typeArgsRange = typeArgsRange) ->
    TypeUseConvention.check src expr typeArgs typeArgsRange
    checkExpression src expr
  | SynExpr.ObjExpr(bindings = bindings; members = members) ->
    checkBindings src LowerCamelCase bindings
    checkMemberDefns src members
  | SynExpr.ComputationExpr(expr = expr) ->
    checkExpression src expr
  | SynExpr.New(targetType = targetType; expr = expr) ->
    TypeConstructor.checkConstructorSpacing src targetType expr
    checkExpression src expr
  | SynExpr.LongIdentSet(expr = expr) ->
    checkExpression src expr
  | SynExpr.DotIndexedSet(objectExpr = objectExpr
                          indexArgs = indexArgs
                          valueExpr = valueExpr) ->
    checkExpression src objectExpr
    checkExpression src indexArgs
    checkExpression src valueExpr
  | SynExpr.DotLambda(expr = expr) ->
    checkExpression src expr
  | SynExpr.Record(copyInfo = copyInfo
                   recordFields = recordFields
                   range = range) ->
    RecordConvention.checkConstructor src copyInfo recordFields range
    for recordField in recordFields do
      let SynExprRecordField(expr = expr) = recordField
      if expr.IsSome then checkExpression src expr.Value
      else ()
  | SynExpr.Lazy(expr = expr) ->
    checkExpression src expr
  | SynExpr.AddressOf _
  | SynExpr.Assert _
  | SynExpr.DotIndexedGet _
  | SynExpr.DotNamedIndexedPropertySet _
  | SynExpr.Fixed _
  | SynExpr.Ident _
  | SynExpr.IndexRange _
  | SynExpr.InterpolatedString _
  | SynExpr.LongIdent _
  | SynExpr.NamedIndexedPropertySet _
  | SynExpr.Null _
  | SynExpr.ObjExpr _
  | SynExpr.Set _
  | SynExpr.YieldOrReturn _
  | SynExpr.YieldOrReturnFrom _ ->
    () (* no need to check this *)
  | expr ->
    failwith $"{nameof checkExpression} TODO: {expr}"

and checkIdOpt src case = function
  | Some(id: Ident) ->
    IdentifierConvention.check src case true id.idText id.idRange
  | None -> failwith "?"

and checkMemberDefns src members =
  for memberDefn in members do
    match memberDefn with
    | SynMemberDefn.Member(binding, _) ->
      checkBinding src PascalCase binding
    | SynMemberDefn.GetSetMember(get, set, _, _) ->
      if get.IsSome then checkBinding src PascalCase get.Value else ()
      if set.IsSome then checkBinding src PascalCase set.Value else ()
    | SynMemberDefn.LetBindings(bindings = bindings) ->
      checkBindings src LowerCamelCase bindings
    | SynMemberDefn.AbstractSlot(slotSig = SynValSig(ident = id
                                                     synType = synType)) ->
      let SynIdent(ident = id) = id
      TypeAnnotation.checkAbstractSlot src id synType
      TypeUseConvention.checkTypeAbbrevWithAnnotation src synType
      IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynMemberDefn.Interface(members = Some members) ->
      checkMemberDefns src members
    | SynMemberDefn.ValField(SynField(idOpt = idOpt), _) ->
      checkIdOpt src PascalCase idOpt
    | SynMemberDefn.AutoProperty(ident = id; typeOpt = typ; synExpr = expr) ->
      TypeAnnotation.checkMember src id typ
      IdentifierConvention.check src PascalCase true id.idText id.idRange
      checkExpression src expr
    | SynMemberDefn.ImplicitInherit(inheritArgs = inheritArgs) ->
      checkExpression src inheritArgs
    | SynMemberDefn.ImplicitCtor _
    | SynMemberDefn.Inherit _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkMemberDefns} TODO: {memberDefn}"

and checkTypeDefnSimpleRepr src trivia = function
  | SynTypeDefnSimpleRepr.Union(unionCases = cases) ->
    TypeAnnotation.checkUnionType src cases
    TypeUseConvention.checkUnionType src cases
    for case in cases do
      let SynUnionCase(ident = SynIdent(ident = id); range = range) = case
      IdentifierConvention.check src PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Enum(cases = cases) ->
    for case in cases do
      let SynEnumCase(ident = SynIdent(ident = id); range = range) = case
      IdentifierConvention.check src PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Record(recordFields = fields; range = range) ->
    TypeAnnotation.checkSynFields src fields
    RecordConvention.checkDefinition src fields range trivia
    for field in fields do
      let SynField(idOpt = idOpt; fieldType = fieldType) = field
      TypeUseConvention.checkTypeAbbrevWithAnnotation src fieldType
      checkIdOpt src PascalCase idOpt
  | SynTypeDefnSimpleRepr.Exception repr ->
    checkExceptionDefnRepr src repr
  | SynTypeDefnSimpleRepr.TypeAbbrev(rhsType = rhsType) ->
    TypeUseConvention.checkTypeAbbrevWithAnnotation src rhsType
  | SynTypeDefnSimpleRepr.None _ ->
    () (* no need to check this *)
  | repr ->
    failwith $"{nameof checkTypeDefnSimpleRepr} TODO: {repr}"

and checkExceptionDefnRepr src repr =
  let SynExceptionDefnRepr(caseName = caseName) = repr
  let SynUnionCase(ident = SynIdent(ident = id); range = range) = caseName
  IdentifierConvention.check src PascalCase true id.idText range

and checkTypeDefnRepr src lid repr trivia =
  match repr with
  | SynTypeDefnRepr.ObjectModel(_, members, _) ->
    ClassDefinition.checkIdentifierWithParen src members
    ClassMemberConvention.checkMemberOrder src members
    checkMemberDefns src members
  | SynTypeDefnRepr.Simple(repr, _) ->
    checkTypeDefnSimpleRepr src trivia repr
  | SynTypeDefnRepr.Exception repr ->
    checkExceptionDefnRepr src repr

and checkTypeDefn src defn =
  let SynTypeDefn(typeInfo = info
                  typeRepr = repr
                  members = members
                  implicitConstructor = implicitConstructor
                  trivia = trivia) = defn
  let SynComponentInfo(longId = lid; range = range; attributes = attrs) = info
  let name = (List.last lid).idText
  if hasAttr "Measure" attrs then ()
  else IdentifierConvention.check src PascalCase true name range
  if implicitConstructor.IsSome then
    ClassDefinition.checkIdentifierWithParen src [ implicitConstructor.Value ]
  else
    ()
  checkTypeDefnRepr src lid repr trivia
  checkMemberDefns src members

and hasAttr attrName attrs =
  attrs
  |> List.exists (fun (lst: SynAttributeList) ->
    lst.Attributes
    |> List.exists (fun attr ->
      let SynLongIdent(id = lid) = attr.TypeName
      lid |> List.exists (fun id -> id.idText = attrName)
    )
  )

and checkBinding src case binding =
  let SynBinding(headPat = pat
                 expr = body
                 attributes = attrs
                 returnInfo = returnInfo
                 trivia = trivia) = binding
  let case = if hasAttr "Literal" attrs then PascalCase else case
  checkPattern src case false trivia pat
  DeclarationConvention.checkEqualSpacing src trivia.EqualsRange
  DeclarationConvention.checkLetAndMultilineRhsPlacement src binding
  DeclarationConvention.checkUnnecessaryLineBreak src binding
  DeclarationConvention.checkComputationExprPlacement src binding
  TypeUseConvention.checkParamTypeSpacing src pat
  TypeAnnotation.checkReturnInfo src pat returnInfo
  PatternMatchingConvention.checkBody src pat
  ClassMemberConvention.checkSelfIdentifierUsage src pat body
  TypeUseConvention.checkExprAnnotation src body
  checkExpression src body

and checkBindings src case bindings =
  for binding in bindings do
    checkBinding src case binding

and checkTypeDefnWithContext
 (src: ISourceText) (context: CheckContext) (typeDefn: SynTypeDefn) =
  let (SynTypeDefn(typeInfo = componentInfo;
                   typeRepr = repr;
                   members = explicitMembers;
                   range = range)) = typeDefn
  let (SynComponentInfo(accessibility = typeAccess)) = componentInfo
  let effectiveTypeAccess =
    match typeAccess with
    | Some _ -> AccessModifierConvention.getAccessLevel typeAccess
    | None -> context.ModuleAccess
  match typeAccess with
  | Some _ ->
    let explicitAccess = AccessModifierConvention.getAccessLevel typeAccess
    if explicitAccess <= context.ModuleAccess then
      let scopeContext: AccessModifierConvention.ScopeContext =
        { AccessModifierConvention.ModuleAccess = context.ModuleAccess
          AccessModifierConvention.TypeAccess = None }
      AccessModifierConvention.checkTypeInModule
       src scopeContext typeAccess range
  | None -> ()
  let allMembers =
    match repr with
    | SynTypeDefnRepr.ObjectModel(_, reprMembers, _) -> reprMembers
    | _ -> []
  let allMembers = allMembers @ explicitMembers
  let memberScopeContext: AccessModifierConvention.ScopeContext =
    { AccessModifierConvention.ModuleAccess = context.ModuleAccess
      AccessModifierConvention.TypeAccess = Some effectiveTypeAccess }
  for memberDefn in allMembers do
    AccessModifierConvention.checkTypeMember src memberScopeContext memberDefn
  checkTypeDefn src typeDefn

and checkDeclarationsWithContext
 (src: ISourceText) (context: CheckContext) (decls: SynModuleDecl list) =
  DeclarationConvention.check src decls
  for decl in decls do
    match decl with
    | SynModuleDecl.ModuleAbbrev(ident = id) ->
      IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynModuleDecl.NestedModule(
      moduleInfo = info; decls = decls; range = range) ->
      let SynComponentInfo(longId = lid; accessibility = access) = info
      for id in lid do
        IdentifierConvention.check src PascalCase true id.idText id.idRange
      let effectiveModuleAccess =
        match access with
        | Some _ -> AccessModifierConvention.getAccessLevel access
        | None -> context.ModuleAccess
      match access with
      | Some _ ->
        let explicitAccess = AccessModifierConvention.getAccessLevel access
        if explicitAccess <= context.ModuleAccess then
          let scopeContext: AccessModifierConvention.ScopeContext =
            { AccessModifierConvention.ModuleAccess = context.ModuleAccess
              AccessModifierConvention.TypeAccess = None }
          AccessModifierConvention.checkNestedModule
           src scopeContext access range
      | None -> ()
      let nestedContext = { ModuleAccess = effectiveModuleAccess }
      checkDeclarationsWithContext src nestedContext decls
    | SynModuleDecl.Let(_, bindings, range) ->
      let scopeContext: AccessModifierConvention.ScopeContext =
        { AccessModifierConvention.ModuleAccess = context.ModuleAccess
          AccessModifierConvention.TypeAccess = None }
      for binding in bindings do
        AccessModifierConvention.checkLetBinding src scopeContext binding
      FunctionBodyConvention.checkBindings src range bindings
      checkBindings src LowerCamelCase bindings
    | SynModuleDecl.Expr(expr = expr) ->
      checkExpression src expr
    | SynModuleDecl.Types(typeDefns, range) ->
      if typeDefns.Length > 1 then
        ClassDefinition.checkNestedTypeDefns src range typeDefns
      else
        ()
      for typeDefn in typeDefns do
        checkTypeDefnWithContext src context typeDefn
    | SynModuleDecl.Open _
    | SynModuleDecl.HashDirective _
    | SynModuleDecl.Exception _
    | SynModuleDecl.Attributes _ ->
      () (* no need to check this *)
    | _ ->
      failwith $"{nameof checkDeclarations} TODO: {decl}"

and checkDeclarations (src: ISourceText) (decls: SynModuleDecl list) =
  checkDeclarationsWithContext src defaultCheckContext decls

let checkWithAST src input =
  match input with
  | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
    for m in modules do
      let SynModuleOrNamespace(longId = lid;
                                decls = decls;
                                accessibility = access) = m
      for id in lid do
        IdentifierConvention.check src PascalCase true id.idText id.idRange
      let moduleAccess = AccessModifierConvention.getAccessLevel access
      let context = { ModuleAccess = moduleAccess }
      checkDeclarationsWithContext src context decls
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
  linter.Lint(path, txt)

let runLinter linter (path: string) =
  try lintFile linter path
  with LintException msg ->
    System.Console.WriteLine msg
    exit 1

let linterForFs =
  { new ILintable with
      member _.Lint(path, txt) =
        LineConvention.check txt
        parseFile txt path ||> checkWithAST }

let linterForProjSln =
  { new ILintable with
      member _.Lint(_path, txt) =
        LineConvention.checkWindowsLineEndings txt }

type LintOutcome =
  { Index: int
    Path: string
    Ok: bool
    Log: string }

/// Collects all .fs source files under the given root directory
let getFsFiles (root: string) =
  let sep = Path.DirectorySeparatorChar |> string |> Regex.Escape
  let exclusion =
    [| Regex $"obj{sep}Debug{sep}"
       Regex $"obj{sep}Release{sep}"
       Regex $"CFG.Tests" |]
  Directory.EnumerateFiles(root, "*.fs", SearchOption.AllDirectories)
  |> Seq.filter
    (fun f -> not (exclusion |> Array.exists (fun r -> r.IsMatch f)))
  |> Seq.sort
  |> Seq.toArray

/// Collects .fsproj and .sln project/solution files
let getProjOrSlnFiles (root: string) =
  seq {
    yield!
      Directory.EnumerateFiles(root, "*.fsproj", SearchOption.AllDirectories)
    yield!
      Directory.EnumerateFiles(root, "*.sln", SearchOption.AllDirectories)
  }
  |> Seq.sort
  |> Seq.toArray

/// Lints a single file, catching exceptions and returning a `LintOutcome`.
let tryLintToBuffer
  (linter: ILintable) (index: int) (path: string): LintOutcome =
  let sb = StringBuilder()
  let append (s: string) = sb.AppendLine(s) |> ignore
  try
    Console.WriteLine($"--- File: {path}")
    append $"Linting file: {path}"
    Utils.setCurrentFile path
    let bytes = File.ReadAllBytes path |> ensureNoBOM
    let txt = System.Text.Encoding.UTF8.GetString bytes
    linter.Lint(path, txt)
    { Index = index; Path = path; Ok = true; Log = sb.ToString() }
  with
    LintException msg ->
      Console.WriteLine($"--- File: {path}")
      append msg
      { Index = index; Path = path; Ok = false; Log = sb.ToString() }

/// Runs linting jobs in parallel for all given files
let runParallelPreservingOrder (linter: ILintable) (paths: string array) =
  let jobs =
    paths
    |> Array.mapi (fun i p -> async { return tryLintToBuffer linter i p })
  let results =
    jobs
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.sortBy (fun r -> r.Index)
  results |> Array.exists (fun r -> not r.Ok)

[<EntryPoint>]
let main args =
  if args.Length < 1 then exitWithError "Usage: fslint <file|dir>"
  elif File.Exists args[0] then
    Utils.setCurrentFile args[0]
    let outcome = tryLintToBuffer linterForFs 0 args[0]
    if outcome.Ok then 0 else 1
  elif Directory.Exists args[0] then
    let projOrSln = getProjOrSlnFiles args[0]
    for p in projOrSln do
      Utils.setCurrentFile p
      let bytes = File.ReadAllBytes p |> ensureNoBOM
      let txt = System.Text.Encoding.UTF8.GetString bytes
      linterForProjSln.Lint(p, txt)
    let fsFiles = getFsFiles args[0]
    let hasErrors = runParallelPreservingOrder linterForFs fsFiles
    if hasErrors then 1 else 0
  else
    exitWithError $"File or directory '{args[0]}' not found"