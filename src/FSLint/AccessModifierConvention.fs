module B2R2.FSLint.AccessModifierConvention

open FSharp.Compiler.Syntax

let private isRedundant parentAccess memberAccess =
  parentAccess = Private && memberAccess = Private

let checkLetBinding src (context: ScopeContext) (binding: SynBinding) =
  let SynBinding(headPat = pat; range = range) = binding
  match pat with
  | SynPat.LongIdent(accessibility = acc) ->
    acc
    |> Option.iter (fun _ ->
      let memberAccess = getAccessLevel acc
      if isRedundant context.ModuleAccess memberAccess then
        "module" |> reportRedundant src range
      else
        ()
    )
  | _ -> ()

let checkTypeMember src (context: ScopeContext) (memberDefn: SynMemberDefn) =
  match memberDefn with
  | SynMemberDefn.Member(SynBinding(headPat = pat; range = range), _) ->
    match pat, context.TypeAccess with
    | SynPat.LongIdent(accessibility = Some acc), Some typeAccess ->
      let memberAccess = getAccessLevel (Some acc)
      if isRedundant typeAccess memberAccess then
        "type" |> reportRedundant src range
      else
        ()
    | _ -> ()
  | _ -> ()

let checkNestModule src (context: ScopeContext) access range =
  access
  |> Option.iter (fun _ ->
    let moduleAccess = getAccessLevel access
    if isRedundant context.ModuleAccess moduleAccess then
      "module" |> reportRedundant src range
    else
      ()
  )

let checkTypeModule src (context: ScopeContext) access range =
  access
  |> Option.iter (fun _ ->
    let typeAccess = getAccessLevel access
    if isRedundant context.ModuleAccess typeAccess then
      "module" |> reportRedundant src range
    else
      ()
  )
