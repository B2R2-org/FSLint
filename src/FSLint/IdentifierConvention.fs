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

  let private caseCheck style (identifier: string) (range: range) =
    match style with
    | LowerCamelCase ->
      if Char.IsLower identifier[0] || identifier[0] = '_' then ()
      else raiseWithError $"{range} '{identifier}' is not in {LowerCamelCase}."
    | PascalCase ->
      if Char.IsUpper identifier[0] || identifier[0] = '_' then ()
      else raiseWithError $"{range} '{identifier}' is not in {PascalCase}."

  let private underscoreCheck (identifier: string) (range: range) =
    if identifier[1..].Contains "_" then
      raiseWithError $"{range} '{identifier}' contains underscore(s)."

  let check style checkUnderscore (identifier: string) (range: range) =
    if identifier.Contains " " || isKnownKeyWord identifier then ()
    else
      caseCheck style identifier range
      if checkUnderscore then underscoreCheck identifier range
      else ()