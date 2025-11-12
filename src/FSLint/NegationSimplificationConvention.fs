module B2R2.FSLint.NegationSimplificationConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let getOppositeOperator = function
  | "op_Equality" -> Some("op_Inequality", "=", "<>")
  | "op_Inequality" -> Some("op_Equality", "<>", "=")
  | "op_GreaterThan" -> Some("op_LessThanOrEqual", ">", "<=")
  | "op_GreaterThanOrEqual" -> Some("op_LessThan", ">=", "<")
  | "op_LessThan" -> Some("op_GreaterThanOrEqual", "<", ">=")
  | "op_LessThanOrEqual" -> Some("op_GreaterThan", "<=", ">")
  | _ -> None

let extractComparisonOperator = function
  | SynExpr.App(funcExpr = SynExpr.App(funcExpr = funcExpr;); argExpr = _) ->
    match funcExpr with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ id ])) ->
      Some id.idText
    | SynExpr.Ident(ident = id) ->
      Some id.idText
    | _ -> None
  | _ -> None

let check (src: ISourceText) (expr: SynExpr) =
  match expr with
  | SynExpr.App(funcExpr = SynExpr.Ident(ident = notId);
                argExpr = SynExpr.Paren(expr = innerExpr);
                range = range) when notId.idText = "not" ->
    match extractComparisonOperator innerExpr with
    | Some opName ->
      match getOppositeOperator opName with
      | Some(_, originalSymbol, oppositeSymbol) ->
        reportError src range
          (sprintf "Use '%s' instead of 'not (%s)' for better readability"
            oppositeSymbol originalSymbol)
      | None -> ()
    | None -> ()
  | _ -> ()