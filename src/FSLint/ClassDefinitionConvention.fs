module B2R2.FSLint.ClassDefinition

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Diagnostics

let checkMultiLineIdentWithParen (src: ISourceText) ctorRange spaceRange =
  src.GetLineString((ctorRange: range).StartLine - 1)
  |> fun str ->
    if src.GetSubTextFromRange(ctorRange).TrimStart()[0] = str.TrimStart()[0]
    then
      ()
    else
      let subStr = str.Substring(ctorRange.StartColumn - 1, 2)
      if subStr.StartsWith ' ' then reportPascalCaseError src spaceRange else ()

let checkIdentifierWithParen (src: ISourceText) members =
  members
  |> List.iter (fun memberDefn ->
    match memberDefn with
    | SynMemberDefn.ImplicitCtor(accessibility = accessibility
                                 ctorArgs = ctorArgs
                                 range = range) ->
      match accessibility with
      | Some(SynAccess.Internal idRange)
      | Some(SynAccess.Public idRange)
      | Some(SynAccess.Private idRange) ->
        if idRange.EndColumn <> ctorArgs.Range.StartColumn then
          Range.mkRange "" idRange.End ctorArgs.Range.Start
          |> fun wRange -> reportPascalCaseError src wRange
        else ()
      | _ ->
        Range.mkRange "" range.End ctorArgs.Range.Start
        |> checkMultiLineIdentWithParen src ctorArgs.Range
    | SynMemberDefn.ImplicitInherit(inheritType = inheritType
                                    inheritArgs = inheritArgs) ->
      if inheritType.Range.EndColumn <> inheritArgs.Range.StartColumn then
        Range.mkRange "" inheritType.Range.End inheritArgs.Range.Start
        |> fun wRange -> reportPascalCaseError src wRange
      else
        ()
    | SynMemberDefn.Inherit(baseType = baseType; asIdent = ident)
      when baseType.IsSome && ident.IsSome ->
      if baseType.Value.Range.EndColumn <> ident.Value.idRange.StartColumn then
        Range.mkRange "" baseType.Value.Range.End ident.Value.idRange.Start
        |> fun wRange -> reportPascalCaseError src wRange
      else
        ()
    | _ -> ()
  )

let checkNestedTypeDefns (src: ISourceText) (range: range) typeDefns =
  typeDefns
  |> List.skip 1
  |> List.map (fun (typeDefn: SynTypeDefn) -> typeDefn.Range.StartLine - 1)
  |> List.iter (fun recurseIdx ->
    if src.GetLineString(recurseIdx - 1) <> ""
    then reportWarn src range "Add blank line before nested"
    else ()
  )