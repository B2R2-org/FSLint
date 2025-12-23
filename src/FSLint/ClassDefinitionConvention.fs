module B2R2.FSLint.ClassDefinition

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Diagnostics

let checkMultiLineIdentWithParen (src: ISourceText) (ctorRange: range) =
  src.GetLineString(ctorRange.StartLine - 1)
  |> fun str ->
    if src.GetSubTextFromRange(ctorRange).TrimStart()[0] = str.TrimStart()[0]
    then
      ()
    else
      let subStr = str.Substring(ctorRange.StartColumn - 1, 2)
      if subStr.StartsWith ' ' then reportPascalCaseError src ctorRange else ()

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
      reportWarn src range "Nested should be separated by exactly one space."
    else
      ()
  )