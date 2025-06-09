namespace B2R2.FSLint

type CaseStyle =
  | LowerCamelCase
  | PascalCase

module IdentifierConvention =
  open System
  open FSharp.Compiler.Text

  let private keywords =
    [| "new" |]

  let private isKnownKeyWord (identifier: string) =
    Array.exists (fun kw -> kw = identifier) keywords
    || identifier.StartsWith "op_"
    || identifier.StartsWith "|" (* active patterns *)

  let private caseCheck (src: ISourceText) style (identifier: string)
    (range: range) =
    match style with
    | LowerCamelCase ->
      if Char.IsLower identifier[0] || identifier[0] = '_' then
        ()
      else
        Console.WriteLine (src.GetLineString (range.StartLine - 1))
        Console.WriteLine (String.replicate range.StartColumn " " + "^")
        raiseWithError $"{range} '{identifier}' is not in {LowerCamelCase}."
    | PascalCase ->
      if Char.IsUpper identifier[0] || identifier[0] = '_' then
        ()
      else
        Console.WriteLine (src.GetLineString (range.StartLine - 1))
        Console.WriteLine (String.replicate range.StartColumn " " + "^")
        raiseWithError $"{range} '{identifier}' is not in {PascalCase}."

  let private underscoreCheck src (identifier: string) (range: range) =
    let positionOfUnderscore = identifier[1..].IndexOf '_'
    if positionOfUnderscore <> -1 then
      let errorColumn = range.StartColumn + positionOfUnderscore + 1
      Console.WriteLine ((src: ISourceText).GetLineString (range.StartLine - 1))
      Console.WriteLine (String.replicate errorColumn " " + "^")
      raiseWithError $"{range} '{identifier}' contains underscore(s)."
    else
      ()

  let check src style checkUnderscore (identifier: string) (range: range) =
    if identifier.Contains " " || isKnownKeyWord identifier then ()
    else
      caseCheck src style identifier range
      if checkUnderscore then underscoreCheck src identifier range
      else ()