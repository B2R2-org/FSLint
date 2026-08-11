module B2R2.FSLint.AppConvention

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Diagnostics

let makeSpaceRange (fromRange: range) (toRange: range) =
  Range.mkRange fromRange.FileName fromRange.End toRange.Start

let private tryGetTextBetweenSameLine src leftRange rightRange =
  if findDirectivesBetween leftRange rightRange |> Option.isSome then
    None
  else
    let leftAdjusted =
      combineRangeWithComment leftRange rightRange true leftRange
    let rightAdjusted =
      combineRangeWithComment leftRange rightRange false rightRange
    if leftAdjusted.EndLine <> rightAdjusted.StartLine
      || rightAdjusted.StartColumn < leftAdjusted.EndColumn then
      None
    else
      let line = (src: ISourceText).GetLineString(leftAdjusted.EndLine - 1)
      let gap =
        let size = rightAdjusted.StartColumn - leftAdjusted.EndColumn
        line.Substring(leftAdjusted.EndColumn, size)
      Some(leftAdjusted, rightAdjusted, gap)

let private tryGetOperatorSymbol = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(trivia = trivias)) ->
    trivias
    |> List.tryPick (function
      | Some(IdentTrivia.OriginalNotation op) -> Some op
      | _ -> None)
  | _ ->
    None

let private isOperatorExpr expr = Option.isSome <| tryGetOperatorSymbol expr

let private isUnaryOperatorExpr = function
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ ident ])) ->
    ident.idText = "op_UnaryNegation"
    || ident.idText = "op_UnaryPlus"
    || ident.idText = "op_LogicalNot"
  | _ ->
    false

let private checkUnaryOperatorSpacing src funcExpr (argExpr: SynExpr) =
  if isUnaryOperatorExpr (funcExpr: SynExpr) then
    match tryGetTextBetweenSameLine src funcExpr.Range argExpr.Range with
    | Some(leftAdjusted, rightAdjusted, gap)
      when gap |> Seq.exists Char.IsWhiteSpace ->
      makeSpaceRange leftAdjusted rightAdjusted
      |> fun range ->
        reportWarn src range "Remove whitespace after unary operator"
    | _ ->
      ()
  else
    ()

let private checkInfixSpacing src funcExpr (argExpr: SynExpr) =
  let isEqualityExpr expr =
    match tryGetOperatorSymbol expr with
    | Some "=" -> true
    | _ -> false
  match funcExpr with
  | SynExpr.App(isInfix = true; funcExpr = opExpr; argExpr = leftExpr) ->
    if isEqualityExpr opExpr then
      match tryGetTextBetweenSameLine src leftExpr.Range opExpr.Range with
      | Some(leftAdjusted, eqAdjusted, gap) when gap <> " " ->
        makeSpaceRange leftAdjusted eqAdjusted |> reportEqaulAfterSpacing src
      | _ ->
        ()
      match tryGetTextBetweenSameLine src opExpr.Range argExpr.Range with
      | Some(eqAdjusted, rightAdjusted, gap) when gap <> " " ->
        makeSpaceRange eqAdjusted rightAdjusted |> reportEqaulBeforeSpacing src
      | _ ->
        ()
    elif isOperatorExpr opExpr then
      match tryGetTextBetweenSameLine src leftExpr.Range opExpr.Range with
      | Some(leftAdjusted, opAdjusted, gap) when gap <> " " ->
        makeSpaceRange leftAdjusted opAdjusted |> reportInfixSpacing src
      | _ ->
        ()
      match tryGetTextBetweenSameLine src opExpr.Range argExpr.Range with
      | Some(opAdjusted, rightAdjusted, gap) when gap <> " " ->
        makeSpaceRange opAdjusted rightAdjusted |> reportInfixSpacing src
      | _ ->
        ()
    else
      ()
  | _ ->
    ()

let private checkFuncSpacing src funcExpr (argExpr: SynExpr) =
  if not (argExpr.IsArrayOrListComputed && not argExpr.IsParen)
    && not (isUnaryOperatorExpr funcExpr)
    && not (isOperatorExpr funcExpr)
  then
    match tryGetTextBetweenSameLine src funcExpr.Range argExpr.Range with
    | Some(funcAdjusted, argAdjusted, gap)
      when gap |> Seq.exists Char.IsWhiteSpace ->
      if gap <> " " then
        Range.mkRange "" funcAdjusted.End argAdjusted.Start
        |> fun range -> reportWarn src range "Use single whitespace in func app"
      else
        ()
    | _ ->
      ()
  else
    ()

let checkLambdaArrowSpacing src pat body (trivia: SynExprLambdaTrivia) =
  if Option.isSome trivia.ArrowRange then
    if (pat: range).EndLine = trivia.ArrowRange.Value.StartLine
      && pat.EndColumn + 1 <> trivia.ArrowRange.Value.StartColumn
      && (body: range).StartColumn > pat.StartColumn
      && pat.EndColumn - pat.StartColumn > 1 then
      makeSpaceRange pat trivia.ArrowRange.Value |> reportArrowBeforeSpacing src
    elif body.StartLine = trivia.ArrowRange.Value.StartLine
      && body.StartColumn - 1 <> trivia.ArrowRange.Value.EndColumn
      && body.StartColumn > trivia.ArrowRange.Value.EndColumn then
      makeSpaceRange trivia.ArrowRange.Value body |> reportArrowAfterSpacing src
    else
      ()
  else
    ()

let checkLambdaKeywordSpacing (src: ISourceText) lambdaRange argsRange =
  if (argsRange: range).StartLine = (lambdaRange: range).StartLine then
    let gap = Range.mkRange "" lambdaRange.Start argsRange.Start
    let gapStr = gap |> src.GetSubTextFromRange
    let keywordLen = if gapStr[0..3] = "func" then 8 else 3
    let handleException = gapStr[keywordLen..gapStr.Length - 1]
    if handleException.Length > 1 && handleException[1] <> ' ' then
      ()
    else
      gapStr
      |> String.filter (fun ch -> ch = ' ')
      |> fun ws ->
        if ws.Length <> 1 then
          Range.mkRange ""
            (Position.mkPos lambdaRange.StartLine
              (lambdaRange.StartColumn + keywordLen))
            argsRange.Start
          |> fun range ->
            reportWarn src range "Use single whitespace after Lambda"
        else
          ()
  else
    ()

let rec check src isInfix funcExpr (argExpr: SynExpr) =
  let isInfixAppExpr = function
    | SynExpr.App(isInfix = true) -> true
    | _ -> false
  if isStrict then
    if isInfix || isInfixAppExpr funcExpr then
      checkInfixSpacing src funcExpr argExpr
    else
      checkUnaryOperatorSpacing src funcExpr argExpr
      checkFuncSpacing src funcExpr argExpr
  else
    ()
  match funcExpr with
  | SynExpr.App(isInfix = subInfix; funcExpr = subFunc; argExpr = subArg) ->
    check src subInfix subFunc subArg
  | SynExpr.Paren(expr = innerExpr) ->
    traverseParen src isInfix innerExpr
  | SynExpr.Ident _
  | SynExpr.LongIdent _
  | SynExpr.TypeApp _
  | SynExpr.DotGet _
  | SynExpr.Const _
  | SynExpr.ArrayOrListComputed _
  | SynExpr.DotLambda _ ->
    ()
  | expr ->
    warn $"[AppConvention] TODO(funcExpr): {expr}"
  match argExpr with
  | SynExpr.App(isInfix = subInfix; funcExpr = subFunc; argExpr = subArg) ->
    check src subInfix subFunc subArg
  | SynExpr.Paren(expr = innerExpr) ->
    traverseParen src isInfix innerExpr
  | SynExpr.AddressOf(expr = expr; opRange = opRange) ->
    match tryGetTextBetweenSameLine src opRange expr.Range with
    | Some(_, _, gap) when gap <> "" -> reportInfixSpacing src opRange
    | _ -> ()
  | _ ->
    ()

and traverseParen src isInfix = function
  | SynExpr.App(isInfix = subIsInfix; funcExpr = funcExpr; argExpr = argExpr) ->
    check src subIsInfix funcExpr argExpr
  | SynExpr.Lambda(body = body) ->
    traverseParen src isInfix body
  | _ ->
    ()

/// The name a bitwise operator compiles to, when the expression applies one
/// infix. These are the operators that build one value out of several, and a
/// chain of them is a separator list like any other.
let private bitwiseOperator = function
  | SynExpr.App(funcExpr =
                  SynExpr.App(isInfix = true
                              funcExpr =
                                SynExpr.LongIdent(
                                  longDotId = SynLongIdent(id = [ op ])))) ->
    match op.idText with
    | "op_BitwiseOr" | "op_BitwiseAnd" | "op_ExclusiveOr" -> Some op.idText
    | _ -> None
  | _ ->
    None

/// Flattens a chain of one and the same bitwise operator into its operands.
/// Mixing two of them nests by precedence rather than chaining, so only the
/// one on top is followed.
let private flattenBitwiseChain expr =
  let name = bitwiseOperator expr
  let rec loop acc e =
    match e with
    | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; argExpr = lhs)
                  argExpr = rhs) when bitwiseOperator e = name ->
      loop (rhs :: acc) lhs
    | _ ->
      e :: acc
  if Option.isSome name then loop [] expr else []

/// Notes every proper prefix of the chain, so that none of them is judged
/// again on its own. A chain nests to the left, and its prefixes are the
/// left-hand sides down the spine.
let rec private noteChainPrefixes name (expr: SynExpr) =
  match expr with
  | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; argExpr = lhs)) when
      bitwiseOperator expr = name ->
    noteCoveredChain lhs.Range
    noteChainPrefixes name lhs
  | _ ->
    ()

/// A bitwise chain that would stand on one line has to stand on it. Once it
/// would not, it is left alone: a value built out of bits may be meant as a
/// row of fields, packed across the line, or as a set of flags, one to a
/// line, and nothing in the syntax tells the two apart.
let checkBitwiseChain src (expr: SynExpr) =
  match flattenBitwiseChain expr with
  | _ :: _ :: _ as operands when not (isCoveredChain expr.Range) ->
    noteChainPrefixes (bitwiseOperator expr) expr
    operands
    |> List.map (fun (operand: SynExpr) -> operand.Range)
    |> LineBreakConvention.checkClosesUpOnly src
  | _ ->
    ()
