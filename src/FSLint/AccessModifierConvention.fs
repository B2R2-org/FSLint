module B2R2.FSLint.AccessModifierConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

type AccessLevel =
  | Public = 0
  | Internal = 1
  | Private = 2

type ScopeContext =
    { ModuleAccess: AccessLevel
      TypeAccess: AccessLevel option }

let getAccessLevel = function
  | Some(SynAccess.Public _) -> AccessLevel.Public
  | Some(SynAccess.Internal _) -> AccessLevel.Internal
  | Some(SynAccess.Private _) -> AccessLevel.Private
  | None -> AccessLevel.Public

let getAccessLevelFromAttributes (attrs: SynAttributeList list) =
  let hasPrivate =
    attrs
    |> List.exists (fun attrList ->
      attrList.Attributes
      |> List.exists (fun attr ->
        match attr.TypeName with
        | SynLongIdent(id = [ id ]) when id.idText = "private" -> true
        | _ -> false
      )
    )
  if hasPrivate then AccessLevel.Private else AccessLevel.Public

let defaultContext =
    { ModuleAccess = AccessLevel.Public
      TypeAccess = None }

let isRedundant (parentAccess: AccessLevel) (memberAccess: AccessLevel) =
  memberAccess <= parentAccess

let checkLetBinding src (context: ScopeContext) (binding: SynBinding) =
  let SynBinding(headPat = pat; range = range) = binding
  let access =
    match pat with
    | SynPat.LongIdent(accessibility = acc) -> acc
    | _ -> None
  match access with
  | Some _ ->
    let memberAccess = getAccessLevel access
    if isRedundant context.ModuleAccess memberAccess then
      let accessStr =
        match memberAccess with
        | AccessLevel.Private -> "private"
        | AccessLevel.Internal -> "internal"
        | _ -> "public"
      reportError src range
        (sprintf
          "Redundant '%s' modifier: already restricted by enclosing module"
          accessStr)
    else
      ()
  | None ->
    ()

let checkTypeMember
 (src: ISourceText) (context: ScopeContext) (memberDefn: SynMemberDefn) =
  match memberDefn with
  | SynMemberDefn.Member(binding, _) ->
    let SynBinding(headPat = pat; range = range) = binding
    let access =
      match pat with
      | SynPat.LongIdent(accessibility = acc) -> acc
      | _ -> None
    match access, context.TypeAccess with
    | Some _, Some typeAccess ->
      let memberAccess = getAccessLevel access
      if isRedundant typeAccess memberAccess then
        let accessStr =
          match memberAccess with
          | AccessLevel.Private -> "private"
          | AccessLevel.Internal -> "internal"
          | _ -> "public"
        reportError src range
          (sprintf
            "Redundant '%s' modifier: already restricted by enclosing type"
            accessStr)
      else
        ()
    | _ ->
      ()
  | _ ->
    ()

let checkNestedModule (src: ISourceText) (context: ScopeContext)
                      (access: SynAccess option) (range: range) =
  match access with
  | Some _ ->
    let moduleAccess = getAccessLevel access
    if isRedundant context.ModuleAccess moduleAccess then
      let accessStr =
        match moduleAccess with
        | AccessLevel.Private -> "private"
        | AccessLevel.Internal -> "internal"
        | _ -> "public"
      reportError src range
        (sprintf
          "Redundant '%s' modifier: already restricted by enclosing module"
          accessStr)
    else
      ()
  | None -> ()

let checkTypeInModule (src: ISourceText) (context: ScopeContext)
                      (access: SynAccess option) (range: range) =
  match access with
  | Some _ ->
    let typeAccess = getAccessLevel access
    if isRedundant context.ModuleAccess typeAccess then
      let accessStr =
        match typeAccess with
        | AccessLevel.Private -> "private"
        | AccessLevel.Internal -> "internal"
        | _ -> "public"
      reportError src range
        (sprintf
          "Redundant '%s' modifier: already restricted by enclosing module"
          accessStr)
  | None -> ()

let hasAccessModifier (componentInfo: SynComponentInfo) =
  let (SynComponentInfo(accessibility = access)) = componentInfo
  access

let getAccessFromComponentInfo (componentInfo: SynComponentInfo) =
  let (SynComponentInfo(accessibility = access)) = componentInfo
  getAccessLevel access