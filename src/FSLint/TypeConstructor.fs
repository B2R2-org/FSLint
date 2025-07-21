module B2R2.FSLint.TypeConstructor

open FSharp.Compiler.Syntax

/// Checks if there is incorrect spacing in constructor calls.
let checkConstructorSpacing src targetType = function
  | SynExpr.Const (SynConst.Unit, range)
  | SynExpr.Paren (range = range) ->
    match targetType with
    | SynType.LongIdent (longDotId = SynLongIdent (id = id)) ->
      match id with
      | [ id ] when FunctionCallConvention.isPascalCase id.idText ->
        if id.idRange.EndColumn <> range.StartColumn then
          reportError src range "Contains invalid whitespace"
        else ()
      | _ -> ()
    | _ -> ()
  | _ -> ()
