module B2R2.FSLint.AccessModifierConvention

open FSharp.Compiler.Syntax

let private isRedundant parentAccess memberAccess =
  parentAccess = Private && memberAccess = Private

let checkLetBinding src (binding: SynBinding) (context: ScopeContext) =
  let SynBinding(headPat = pat) = binding
  match pat with
  | SynPat.LongIdent(accessibility = acc) ->
    acc
    |> Option.iter (fun _ ->
      let memberAccess = getAccessLevel acc
      if isRedundant context.ModuleAccess memberAccess then
        reportRedundant src acc.Value.Range
      else
        ()
    )
  | _ -> ()

let checkTypeMember src (context: ScopeContext) (memberDefn: SynMemberDefn) =
  match memberDefn with
  | SynMemberDefn.Member(SynBinding(headPat = pat), _) ->
    match pat, context.TypeAccess with
    | SynPat.LongIdent(accessibility = Some acc), Some typeAccess ->
      let memberAccess = getAccessLevel (Some acc)
      if isRedundant typeAccess memberAccess then reportRedundant src acc.Range
      else ()
    | _ -> ()
  | _ -> ()

let checkNestModule src access range (context: ScopeContext) =
  access
  |> Option.iter (fun _ ->
    let moduleAccess = getAccessLevel access
    if isRedundant context.ModuleAccess moduleAccess then
      reportRedundant src range
    else
      ()
  )

let checkTypeModule src (context: ScopeContext) access =
  access
  |> Option.iter (fun _ ->
    let typeAccess = getAccessLevel access
    if isRedundant context.ModuleAccess typeAccess then
      reportRedundant src access.Value.Range
    else
      ()
  )
