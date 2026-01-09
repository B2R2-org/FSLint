module B2R2.FSLint.TypeConstructor

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

let checkAsSpacing src (idRange: range) (asRange: range) (selfRange: range) =
  if idRange.EndLine = asRange.StartLine
    && idRange.EndColumn + 1 <> asRange.StartColumn then
      Range.mkRange "" idRange.End asRange.Start
      |> fun range -> reportWarn src range "Use single whitespace before 'as'"
  elif asRange.EndLine = selfRange.StartLine
    && asRange.EndColumn + 1 <> selfRange.StartColumn then
      Range.mkRange "" asRange.End selfRange.Start
      |> fun range -> reportWarn src range "Use single whitespace after 'as'"
  else
    ()

let checkEqualSpacing src (idRange: range) (reprRange: range) equalRange =
  if Option.isNone equalRange then ()
  else
    if idRange.EndLine = (equalRange.Value: range).StartLine
      && idRange.EndColumn + 1 <> equalRange.Value.StartColumn then
      Range.mkRange "" idRange.End equalRange.Value.Start
      |> fun range ->
        reportWarn src range "Use single whitespace before '='"
    elif reprRange.StartLine = equalRange.Value.EndLine
      && equalRange.Value.EndColumn + 1 <> reprRange.StartColumn then
      Range.mkRange "" equalRange.Value.End reprRange.Start
      |> fun range ->
        reportWarn src range "Use single whitespace after '='"
    else
      ()

/// Checks if there is incorrect spacing in constructor calls.
let checkConstructorSpacing src targetType = function
  | SynExpr.Const(SynConst.Unit, range)
  | SynExpr.Paren(range = range) ->
    match targetType with
    | SynType.Var(typar = SynTypar(id, TyparStaticReq.None, false)) ->
      if isPascalCase id.idText then
        if id.idRange.EndColumn <> range.StartColumn then
          Range.mkRange "" id.idRange.End range.Start
          |> reportPascalCaseError src
        else
          ()
      elif id.idRange.EndColumn + 1 <> range.StartColumn then
        Range.mkRange "" id.idRange.End range.Start
        |> reportLowerCaseError src
      else
        ()
    | SynType.Var _ as typ ->
      warn $"[Constructor]TODO: {typ}"
    | SynType.LongIdent(longDotId = SynLongIdent(id = id)) ->
      let id = List.last id
      if isPascalCase id.idText
        && id.idRange.EndColumn <> range.StartColumn
      then
        Range.mkRange "" id.idRange.End range.Start
        |> reportPascalCaseError src
      else
        ()
    | _ ->
      ()
  | _ ->
    ()
