module B2R2.FSLint.AccessModifierConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

type AccessLevel =
  | Public
  | Private

type ScopeContext =
  { ModuleAccess: AccessLevel
    TypeAccess: AccessLevel option }

let getAccessLevel = function
  | Some(SynAccess.Private _) -> Private
  | _ -> Public

let isRedundant parentAccess memberAccess =
  parentAccess = Private && memberAccess = Private

let accessToString = function
  | Private -> "private"
  | Public -> "public"

let reportRedundant src range accessLevel scopeType =
  let accessStr = accessToString accessLevel
  reportError src range
    ("Redundant '" + accessStr + "' modifier: already restricted by " +
     "enclosing " + scopeType)

let getPatternAccess = function
  | SynPat.LongIdent(accessibility = acc) -> acc
  | _ -> None

let checkLetBinding src (context: ScopeContext) (binding: SynBinding) =
  let SynBinding(headPat = pat; range = range) = binding
  getPatternAccess pat
  |> Option.iter (fun _ ->
    let memberAccess = getAccessLevel (getPatternAccess pat)
    if isRedundant context.ModuleAccess memberAccess then
      reportRedundant src range Private "module"
  )

let checkTypeMember src (context: ScopeContext) (memberDefn: SynMemberDefn) =
  match memberDefn with
  | SynMemberDefn.Member(SynBinding(headPat = pat; range = range), _) ->
    match getPatternAccess pat, context.TypeAccess with
    | Some _, Some typeAccess ->
      let memberAccess = getAccessLevel (getPatternAccess pat)
      if isRedundant typeAccess memberAccess then
        reportRedundant src range Private "type"
    | _ -> ()
  | _ -> ()

let checkNestedModule src (context: ScopeContext) access range =
  access
  |> Option.iter (fun _ ->
    let moduleAccess = getAccessLevel access
    if isRedundant context.ModuleAccess moduleAccess then
      reportRedundant src range Private "module"
  )

let checkTypeInModule src (context: ScopeContext) access range =
  access
  |> Option.iter (fun _ ->
    let typeAccess = getAccessLevel access
    if isRedundant context.ModuleAccess typeAccess then
      reportRedundant src range Private "module"
  )