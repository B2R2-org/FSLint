module B2R2.FSLint.NegationSimplificationConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let private getOppositeOperator = function
  | "op_Equality" -> Some("op_Inequality", "=", "<>")
  | "op_Inequality" -> Some("op_Equality", "<>", "=")
  | "op_GreaterThan" -> Some("op_LessThanOrEqual", ">", "<=")
  | "op_GreaterThanOrEqual" -> Some("op_LessThan", ">=", "<")
  | "op_LessThan" -> Some("op_GreaterThanOrEqual", "<", ">=")
  | "op_LessThanOrEqual" -> Some("op_GreaterThan", "<=", ">")
  | _ -> None

let private extractComparisonOperator = function
  | SynExpr.App(funcExpr = SynExpr.App(funcExpr = funcExpr)) ->
    match funcExpr with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ id ]))
    | SynExpr.Ident(ident = id) -> Some id.idText
    | _ -> None
  | _ -> None

let private checkParenthesizedNegation (src: ISourceText) = function
  | SynExpr.App(funcExpr = SynExpr.Ident(ident = notId);
                argExpr = SynExpr.Paren(expr = innerExpr);
                range = range) when notId.idText = "not" ->
    match extractComparisonOperator innerExpr with
    | Some opName ->
      match getOppositeOperator opName with
      | Some(_, oriSymbol, oppSymbol) ->
        reportError src range
          $"Use '{oppSymbol}' instead of 'not ({oriSymbol})'"
      | None -> ()
    | None -> ()
  | _ -> ()

let private checkPipelineNegation (src: ISourceText) = function
  | SynExpr.App(
    funcExpr = SynExpr.App(
      funcExpr = SynExpr.LongIdent(
        longDotId = SynLongIdent(id = [ pipeId ]));
      argExpr = comparisonExpr);
    argExpr = SynExpr.Ident(ident = notId);
    range = range)
    when pipeId.idText = "op_PipeRight" && notId.idText = "not" ->
    let innerExpr =
      match comparisonExpr with
      | SynExpr.Paren(expr = inner) -> inner
      | _ -> comparisonExpr
    match extractComparisonOperator innerExpr with
    | Some opName ->
      match getOppositeOperator opName with
      | Some(_, oriSymbol, oppSymbol) ->
        reportError src range
          $"Use '{oppSymbol}' instead of '({oriSymbol}) |> not'"
      | None -> ()
    | None -> ()
  | _ -> ()

let check (src: ISourceText) (expr: SynExpr) =
  checkParenthesizedNegation src expr
  checkPipelineNegation src expr