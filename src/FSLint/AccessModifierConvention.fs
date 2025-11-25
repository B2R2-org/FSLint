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

let reportRedundant src range accessLevel scopeType =
  let accStr =
    match accessLevel with
    | Private -> "private"
    | Public -> "public"
  reportError src range
    ($"Redundant '{accStr}' modifier: already restricted by " +
     $"enclosing {scopeType}")

let checkLetBinding src (context: ScopeContext) (binding: SynBinding) =
  let SynBinding(headPat = pat; range = range) = binding
  match pat with
  | SynPat.LongIdent(accessibility = acc) ->
    acc
    |> Option.iter (fun _ ->
      let memberAccess = getAccessLevel acc
      if isRedundant context.ModuleAccess memberAccess then
        reportRedundant src range Private "module"
    )
  | _ -> ()

let checkTypeMember src (context: ScopeContext) (memberDefn: SynMemberDefn) =
  match memberDefn with
  | SynMemberDefn.Member(SynBinding(headPat = pat; range = range), _) ->
    match pat, context.TypeAccess with
    | SynPat.LongIdent(accessibility = Some acc), Some typeAccess ->
      let memberAccess = getAccessLevel (Some acc)
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
