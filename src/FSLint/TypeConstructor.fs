module B2R2.FSLint.TypeConstructor

open FSharp.Compiler.Syntax
open Diagnostics

/// Checks if there is incorrect spacing in constructor calls.
let checkConstructorSpacing src targetType = function
  | SynExpr.Const(SynConst.Unit, range)
  | SynExpr.Paren(range = range) ->
    match targetType with
    | SynType.Var(typar = SynTypar(id, TyparStaticReq.None, false)) ->
      if isPascalCase id.idText then
        if id.idRange.EndColumn <> range.StartColumn then
          reportWarn src range "Remove whitespace before '('"
        else
          ()
      elif id.idRange.EndColumn + 1 <> range.StartColumn then
        reportWarn src range "Add whitespace before '('"
      else
        ()
    | SynType.Var _ as typ ->
      warn $"[Constructor]TODO: {typ}"
    | SynType.LongIdent(longDotId = SynLongIdent(id = id)) ->
      let id = List.last id
      if isPascalCase id.idText
        && id.idRange.EndColumn <> range.StartColumn
      then reportWarn src range "Remove whitespace before '('"
      else ()
    | _ ->
      ()
  | _ ->
    ()
