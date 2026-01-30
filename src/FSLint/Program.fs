module B2R2.FSLint.Program

open System
open System.IO
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let rec checkPattern src case isArg (trivia: SynBindingTrivia) = function
  | SynPat.Attrib _
  | SynPat.Const _
  | SynPat.Record _
  | SynPat.Wild _ ->
    () (* no need to check this *)
  | SynPat.Named(ident = SynIdent(ident = id); range = range) ->
    IdentifierConvention.check src case true id.idText range
  | SynPat.Typed(pat = pat; targetType = typ) ->
    checkPattern src case isArg trivia pat
    TypeAnnotation.checkPat src pat typ
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
  | SynPat.LongIdent(lid, _, _,
    SynArgPats.NamePatPairs(pats = pat), _, range) ->
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
  let SynMatchClause(pat = pat
                     whenExpr = whenExpr
                     resultExpr = expr
                     trivia = trivia) = clause
  if Option.isSome trivia.ArrowRange then
    PatternMatchingConvention.checkArrowSpacing
      src pat.Range whenExpr expr.Range trivia.ArrowRange.Value
  else
    ()
  PatternMatchingConvention.checkBody src pat
  TypeAnnotation.checkParamTypeSpacing src pat
  RecordConvention.checkRecordPat src pat
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
  | SynExpr.Lambda(args = args
                   body = body
                   range = lambdaRange
                   parsedData = parsedData
                   trivia = trivia) as t ->
    let SynSimplePats.SimplePats(pats = pats; range = patRange) = args
    for pat in pats do checkSimplePattern src LowerCamelCase pat
    AppConvention.checkLambdaArrowSpacing src patRange body.Range trivia
    AppConvention.checkLambdaKeywordSpacing src lambdaRange args.Range
    if Option.isSome parsedData then
      let pats, expr = parsedData.Value
      for pat in pats do
        ParenConvention.checkPat src pat
        TypeAnnotation.checkFieldWidthByPat src pat
        TypeAnnotation.checkParamTypeSpacing src pat
      checkExpression src body
      checkExpression src expr
    else
      checkExpression src body
  | SynExpr.LetOrUse(bindings = bindings; body = body) ->
    checkBindings src LowerCamelCase bindings
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
                       range = range
                       trivia = trivia) ->
    NegationSimplificationConvention.check src ifExpr
    IfThenElseConvention.check src ifExpr thenExpr elseExpr range trivia
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
                    lessRange = less
                    typeArgs = typeArgs
                    greaterRange = greater
                    typeArgsRange = range) ->
    TypeUseConvention.check src expr typeArgs (Some less) greater range
    checkExpression src expr
  | SynExpr.ObjExpr(bindings = bindings; members = members) ->
    checkBindings src LowerCamelCase bindings
    checkMemberDefns src members false
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
  | SynExpr.YieldOrReturnFrom _
  | SynExpr.DiscardAfterMissingQualificationAfterDot _
  | SynExpr.FromParseError _ ->
    () (* no need to check this *)
  | expr ->
    failwith $"{nameof checkExpression} TODO: {expr}"

and checkIdOpt src case = function
  | Some(id: Ident) ->
    IdentifierConvention.check src case true id.idText id.idRange
  | None -> failwith "?"

and checkMemberDefns src members isDelegate =
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
                                                     synType = synType
                                                     trivia = trivia)) ->
      let SynIdent(ident = id) = id
      if isDelegate then
        ()
      else
        TypeAnnotation.checkAbstractSpacing src id synType
          trivia.LeadingKeyword.Range
      TypeAnnotation.checkAbstractSlot src id synType
      TypeAnnotation.checkTypeAbbrevWithAnnotation src synType
      IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynMemberDefn.Interface(members = Some members) ->
      ClassMemberConvention.checkMemberOrder src members
      checkMemberDefns src members isDelegate
    | SynMemberDefn.ValField(SynField(idOpt = idOpt), _) ->
      checkIdOpt src PascalCase idOpt
    | SynMemberDefn.AutoProperty(ident = id
                                 typeOpt = typ
                                 synExpr = expr
                                 trivia = trivia) ->
      TypeAnnotation.checkMember src id typ
      ClassMemberConvention.checkAutoPropertySpacing src id typ expr trivia
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
  | SynTypeDefnSimpleRepr.Union(unionCases = cases; range = range) ->
    TypeAnnotation.checkUnionType src cases
    TypeUseConvention.checkUnionType src cases
    for case in cases do
      let SynUnionCase(ident = SynIdent(ident = id); range = range) = case
      IdentifierConvention.check src PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Enum(cases = cases) as t ->
    for case in cases do
      let SynEnumCase(ident = SynIdent(ident = id)
                      valueExpr = valueExpr
                      range = range
                      trivia = trivia) = case
      TypeConstructor.checkEqualSpacing src id.idRange valueExpr.Range
        (Some trivia.EqualsRange)
      TypeUseConvention.checkBarAlignment src id.idRange trivia.BarRange
      IdentifierConvention.check src PascalCase false id.idText range
  | SynTypeDefnSimpleRepr.Record(recordFields = fields; range = range) ->
    TypeAnnotation.checkSynFields src fields
    RecordConvention.checkDefinition src fields range trivia
    for field in fields do
      let SynField(idOpt = idOpt; fieldType = fieldType) = field
      TypeAnnotation.checkTypeAbbrevWithAnnotation src fieldType
      checkIdOpt src PascalCase idOpt
  | SynTypeDefnSimpleRepr.Exception repr ->
    checkExceptionDefnRepr src repr
  | SynTypeDefnSimpleRepr.TypeAbbrev(rhsType = rhsType) ->
    TypeAnnotation.checkTypeAbbrevWithAnnotation src rhsType
  | SynTypeDefnSimpleRepr.None _ ->
    () (* no need to check this *)
  | repr ->
    failwith $"{nameof checkTypeDefnSimpleRepr} TODO: {repr}"

and checkExceptionDefnRepr src repr =
  let SynExceptionDefnRepr(caseName = caseName) = repr
  let SynUnionCase(ident = SynIdent(ident = id); range = range) = caseName
  IdentifierConvention.check src PascalCase true id.idText range

and checkTypeDefnRepr src repr trivia =
  match repr with
  | SynTypeDefnRepr.ObjectModel(kind, members, _) ->
    ClassDefinition.checkIdentifierWithParen src members
    ClassMemberConvention.checkMemberOrder src members
    checkMemberDefns src members kind.IsDelegate
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
  let SynComponentInfo(longId = lid
                       typeParams = typeParams
                       range = range
                       attributes = attrs) = info
  let name = (List.last lid).idText
  if Option.isSome typeParams then
    ClassDefinition.checkSynTypar src range typeParams.Value
  else
    ()
  ClassDefinition.checkAttributesLineSpacing src attrs trivia
  if hasAttr "Measure" attrs then ()
  else IdentifierConvention.check src PascalCase true name range
  if Option.isSome implicitConstructor then
    ClassDefinition.checkIdentifierWithParen src [ implicitConstructor.Value ]
    match implicitConstructor with
    | Some(SynMemberDefn.ImplicitCtor(ctorArgs = ctorArgs
                                      selfIdentifier = selfIdentifier
                                      trivia = innerTriv))
      when ctorArgs.IsParen ->
      ParenConvention.checkPat src ctorArgs
      if Option.isSome innerTriv.AsKeyword then
        TypeConstructor.checkAsSpacing src ctorArgs.Range
          innerTriv.AsKeyword.Value selfIdentifier.Value.idRange
        TypeConstructor.checkEqualSpacing src selfIdentifier.Value.idRange
          repr.Range trivia.EqualsRange
      else
        TypeConstructor.checkEqualSpacing src ctorArgs.Range repr.Range
          trivia.EqualsRange
    | _ ->
      ()
  else
    match info with
    | SynComponentInfo(typeParams = Some typeParams)
      when typeParams.IsPostfixList ->
      TypeConstructor.checkEqualSpacing src typeParams.Range repr.Range
        trivia.EqualsRange
    | _ ->
      TypeConstructor.checkEqualSpacing src range repr.Range trivia.EqualsRange
  checkTypeDefnRepr src repr trivia
  checkMemberDefns src members false

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
  TypeAnnotation.checkFieldWidthByPat src pat
  checkPattern src case false trivia pat
  if Option.isSome trivia.EqualsRange
    && trivia.LeadingKeyword.IsNew |> not then
    DeclarationConvention.checkEqualSpacing
      src pat.Range trivia.EqualsRange.Value body.Range returnInfo
  else
    ()
  DeclarationConvention.checkLetAndMultilineRhsPlacement src binding
  DeclarationConvention.checkUnnecessaryLineBreak src binding
  DeclarationConvention.checkComputationExprPlacement src binding
  TypeAnnotation.checkParamTypeSpacing src pat
  TypeAnnotation.checkReturnInfo src pat returnInfo
  PatternMatchingConvention.checkBody src pat
  ClassMemberConvention.checkSelfIdentifierUsage src pat body
  TypeAnnotation.checkExprAnnotation src body
  checkExpression src body

and checkBindings src case bindings =
  for binding in bindings do checkBinding src case binding

and checkTypeDefnWithContext src context typeDefn =
  let SynTypeDefn(typeInfo = componentInfo
                  typeRepr = repr
                  members = explicitMembers) = typeDefn
  let SynComponentInfo(typeParams = typeParams
                       accessibility = typeAccess) = componentInfo
  typeAccess |> Option.iter (fun _ ->
    let explicitAccess = getAccessLevel typeAccess
    if explicitAccess <= context.ModuleAccess then
      let ctx =
        { ScopeContext.ModuleAccess = context.ModuleAccess
          ScopeContext.TypeAccess = None }
      AccessModifierConvention.checkTypeModule src ctx typeAccess
    else
      ())
  let effectiveTypeAccess =
    match typeAccess with
    | Some _ -> getAccessLevel typeAccess
    | None -> context.ModuleAccess
  let memberScopeContext =
    { ScopeContext.ModuleAccess = context.ModuleAccess
      ScopeContext.TypeAccess = Some effectiveTypeAccess }
  (match repr with
   | SynTypeDefnRepr.ObjectModel(_, members, _) ->
     List.append members explicitMembers
   | _ ->
     explicitMembers)
  |> List.iter (AccessModifierConvention.checkTypeMember src memberScopeContext)
  checkTypeDefn src typeDefn

and checkDeclarationsWithContext src decls (context: CheckContext) =
  DeclarationConvention.check src decls
  for decl in decls do
    match decl with
    | SynModuleDecl.ModuleAbbrev(ident = id) ->
      IdentifierConvention.check src PascalCase true id.idText id.idRange
    | SynModuleDecl.NestedModule(moduleInfo = info
                                 decls = dls
                                 range = rg
                                 trivia = trivia) ->
      let SynComponentInfo(attributes = attrs
                           longId = lid
                           accessibility = access) = info
      if Option.isSome trivia.ModuleKeyword then
        DeclarationConvention.checkAttributesLineSpacing src attrs
          trivia.ModuleKeyword.Value
      else
        ()
      for id in lid do
        IdentifierConvention.check src PascalCase true id.idText id.idRange
      match access with
      | Some _ when getAccessLevel access <= context.ModuleAccess ->
        { ModuleAccess = context.ModuleAccess
          TypeAccess = None }
        |> AccessModifierConvention.checkNestModule src access rg
      | _ -> ()
      { ModuleAccess =
          match access with
          | Some _ -> getAccessLevel access
          | None -> context.ModuleAccess }
      |> checkDeclarationsWithContext src dls
    | SynModuleDecl.Let(_, bindings, range) ->
      let scopeContext =
        { ModuleAccess = context.ModuleAccess
          TypeAccess = None }
      for binding in bindings do
        AccessModifierConvention.checkLetBinding src binding scopeContext
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
  checkDeclarationsWithContext src decls { ModuleAccess = Public }

let checkWithAST src = function
  | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
    modules
    |> List.iter (fun m ->
      let SynModuleOrNamespace(longId = lid
                               decls = decls
                               attribs = attribs
                               accessibility = access
                               trivia = trivia) = m
      match trivia.LeadingKeyword with
      | SynModuleOrNamespaceLeadingKeyword.Module range ->
        DeclarationConvention.checkAttributesLineSpacing src attribs range
      | _ -> ()
      for id in lid do
        IdentifierConvention.check src PascalCase true id.idText id.idRange
      { ModuleAccess = getAccessLevel access }
      |> checkDeclarationsWithContext src decls
    )
  | ParsedInput.SigFile _ ->
    () (* ignore fsi files *)

let checkBOM (src: ISourceText) (bs: byte[]) =
  if bs.Length > 3 && bs[0] = 0xEFuy && bs[1] = 0xBBuy && bs[2] = 0xBFuy then
    let firstLine = src.GetLineString(0)
    let range =
      Range.mkRange ""
        (Position.mkPos 1 0)
        (Position.mkPos 1 firstLine.Length)
    reportWarn src range
      "Byte Order Mark (BOM) should be removed from the file."
  else
    ()

let linterForFsWithContext context =
  { new ILintable with
      member _.Lint(path, txt) =
        setCurrentLintContext context
        setCurrentFile path
        let runCheck fn =
          try fn () with LintException _ when context.IsSome -> ()
        runCheck (fun () ->
          let src = SourceText.ofString txt
          if path = FakeFsPath then ()
          else checkBOM src (path |> File.ReadAllBytes)
          match LineConvention.check src txt with
          | Ok() -> parseFile src path |> checkWithAST src
          | _ -> ())
        setCurrentLintContext None }

let linterForFs = linterForFsWithContext None

/// Lints a single file, catching exceptions and returning a `LintOutcome`.
let tryOutputToBuffer (index: int) (path: string): LintOutcome =
  try
    setCurrentFile path
    let bytes = File.ReadAllBytes path
    let txt = System.Text.Encoding.UTF8.GetString bytes
    let context =
      { Errors = []
        Source = SourceText.ofString txt
        FilePath = path }
    linterForFsWithContext(Some context).Lint(path, txt)
    setCurrentLintContext None
    { Index = index
      Path = path
      Ok = List.isEmpty context.Errors
      Log = ""
      Errors = context.Errors }
  with
    LintException msg ->
      { Index = index
        Path = path
        Ok = false
        Log = msg
        Errors = [] }

/// Runs linting jobs in parallel for all given files
let runParallelByOrder (paths: string array) =
  paths
   |> Array.mapi (fun i p -> async { return tryOutputToBuffer i p })
  |> Async.Parallel
  |> Async.RunSynchronously
  |> Array.sortBy (fun r -> r.Index)
  |> fun results ->
    results
    |> Array.iter (fun result ->
    if not result.Ok then
      Console.WriteLine ""
      Console.WriteLine $"--- File: {result.Path}"
      Console.WriteLine $"Linting file: {result.Path}"
      if not (List.isEmpty result.Errors) then
        reportWarns result.Errors result.Path
        Console.WriteLine "Linting errors found"
        Console.WriteLine ""
      elif not (String.IsNullOrEmpty result.Log) then
        Console.WriteLine result.Log
        Console.WriteLine "Linting errors found"
        Console.WriteLine ""
      else
        ()
    else
      Console.WriteLine $"Linting file: {result.Path}"
    )
    results |> Array.exists (fun r -> not r.Ok)

let linterForProjSln =
  { new ILintable with
      member _.Lint(path, txt) =
        let src = SourceText.ofString txt
        if path = FakeFsPath then ()
        else checkBOM src (path |> File.ReadAllBytes)
        LineConvention.checkWindowsLineEndings src txt |> ignore }

[<EntryPoint>]
let main args =
  if args.Length < 1 then
    exitWithError "Usage: fslint <file|dir>"
  elif File.Exists args[0] then
    setCurrentFile args[0]
    let outcome = tryOutputToBuffer 0 args[0]
    if not outcome.Ok then
      Console.WriteLine $"--- File: {outcome.Path}"
      Console.Write outcome.Log
      if outcome.Errors.IsEmpty then ()
      else reportWarns outcome.Errors outcome.Path
      1
    else
      0
  elif Directory.Exists args[0] then
    getProjOrSlnFiles args[0]
    |> Array.iter (fun p ->
      setCurrentFile p
      let bytes = File.ReadAllBytes p
      let txt = System.Text.Encoding.UTF8.GetString bytes
      linterForProjSln.Lint(p, txt)
    )
    if getFsFiles args[0] |> runParallelByOrder then 1 else 0
  else
    exitWithError $"File or directory '{args[0]}' not found"