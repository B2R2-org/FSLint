module B2R2.FSLint.TypeConstructor

open FSharp.Compiler.Syntax

/// Checks if there is incorrect spacing in constructor calls.
let checkConstructorSpacing src targetType = function
  | SynExpr.Const(SynConst.Unit, range)
  | SynExpr.Paren(range = range) ->
    match targetType with
    (* Default constructor *)
    | SynType.Var(typar = SynTypar(id, TyparStaticReq.None, false)) ->
      if FunctionCallConvention.isPascalCase id.idText then
        if id.idRange.EndColumn <> range.StartColumn then
          reportError src range "Contains invalid whitespace"
        else ()
      elif id.idRange.EndColumn + 1 <> range.StartColumn then
        reportError src range "Contains invalid whitespace"
      else ()
    | SynType.Var _ as typ -> warn $"[Constructor]TODO: {typ}"
    | SynType.LongIdent(longDotId = SynLongIdent(id = id)) ->
      let id = List.last id
      if FunctionCallConvention.isPascalCase id.idText
        && id.idRange.EndColumn <> range.StartColumn
      then reportError src range "Contains invalid whitespace"
      else ()
    | _ -> ()
  | _ -> ()
