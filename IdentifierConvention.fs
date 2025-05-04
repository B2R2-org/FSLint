namespace B2R2.FSLint

type CaseStyle =
  | LowerCamelCase
  | PascalCase

module IdentifierConvention =
  open System
  open FSharp.Compiler.Text

  let private caseCheck style (identifier: string) (range: range) =
    match style with
    | LowerCamelCase ->
      if Char.IsLower identifier[0] || identifier[0] = '_' then ()
      else exitWithError $"{range} '{identifier}' is not in {LowerCamelCase}."
    | PascalCase ->
      if Char.IsUpper identifier[0] || identifier[0] = '_' then ()
      else exitWithError $"{range} '{identifier}' is not in {PascalCase}."

  let private underscoreCheck (identifier: string) (range: range) =
    if identifier[1..].Contains "_" then
      exitWithError $"{range} '{identifier}' contains underscore(s)."

  let check style (identifier: string) (range: range) =
    caseCheck style identifier range
    underscoreCheck identifier range