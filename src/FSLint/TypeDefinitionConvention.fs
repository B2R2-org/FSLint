module B2R2.FSLint.TypeDefinition

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let reportPascalCaseError src range =
  reportError src range "No space between ident and paren"

let checkMultiLineIdentWithParen (src: ISourceText) (ctorRange: range) =
  src.GetLineString(ctorRange.StartLine - 1)
  |> fun str ->
    if src.GetSubTextFromRange(ctorRange).TrimStart()[0] = str.TrimStart()[0]
    then
      ()
    else
      let subStr = str.Substring(ctorRange.StartColumn - 1, 2)
      if subStr.StartsWith ' ' then reportPascalCaseError src ctorRange
      else ()

let checkIdentifierWithParen (src: ISourceText) members =
  members
  |> List.iter (fun memberDefn ->
    match memberDefn with
    | SynMemberDefn.ImplicitCtor(accessibility = accessibility
                                 ctorArgs = ctorArgs) ->
      match accessibility with
      | Some(SynAccess.Internal idRange)
      | Some(SynAccess.Public idRange)
      | Some(SynAccess.Private idRange) ->
        if idRange.EndColumn <> ctorArgs.Range.StartColumn then
          reportPascalCaseError src ctorArgs.Range
        else ()
      | _ ->
        checkMultiLineIdentWithParen src ctorArgs.Range
    | SynMemberDefn.ImplicitInherit(inheritType = inheritType
                                    inheritArgs = inheritArgs) ->
      if inheritType.Range.EndColumn <> inheritArgs.Range.StartColumn then
        reportPascalCaseError src inheritArgs.Range
      else
        ()
    | SynMemberDefn.Inherit(baseType = baseType; asIdent = ident)
      when baseType.IsSome && ident.IsSome ->
      if baseType.Value.Range.EndColumn <> ident.Value.idRange.StartColumn then
        reportPascalCaseError src ident.Value.idRange
      else
        ()
    | _ -> ()
  )

let checkNestedTypeDefns (src: ISourceText) (range: range) typeDefns =
  typeDefns
  |> List.skip 1
  |> List.map (fun (typeDefn: SynTypeDefn) ->
    typeDefn.Range.StartLine - 1
  )
  |> List.iter (fun recurseIdx ->
    if src.GetLineString(recurseIdx - 1) <> "" then
      reportError src range "Nested should be separated by exactly one space."
    else
      ()
  )

/// Checks whether discriminated union cases in the given type definitions
/// are separated by newlines, enforcing a convention for formatting DU cases.
let checkSimpleRepr (src: ISourceText) typeDefns =
  typeDefns
  |> List.iter (fun typeDefn ->
    match typeDefn with
    | SynTypeDefn(typeRepr = SynTypeDefnRepr.Simple(simpleRepr = simpleRepr)) ->
      match simpleRepr with
      | SynTypeDefnSimpleRepr.Record(range = range)
      | SynTypeDefnSimpleRepr.Enum(range = range)
      | SynTypeDefnSimpleRepr.Union(range = range) ->
        for idx in range.StartLine - 1 .. range.EndLine - 1 do
          if src.GetLineString idx = "" then
            reportError src range "Remove empty line in type."
      | _ -> ()
    | _ -> ()
  )

let rec check src = function
  | SynModuleDecl.NestedModule(decls = decls) ->
    for decl in decls do check src decl
  | SynModuleDecl.Types(typeDefns, range) when typeDefns.Length > 1 ->
    checkNestedTypeDefns src range typeDefns
  | SynModuleDecl.Types(typeDefns, _) ->
    checkSimpleRepr src typeDefns
  | _ ->
    ()