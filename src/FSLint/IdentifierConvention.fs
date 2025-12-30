module B2R2.FSLint.IdentifierConvention

open System
open FSharp.Compiler.Text
open Diagnostics

let private keywords = [| "new" |]

let private isKnownKeyWord (identifier: string) =
  Array.exists (fun kw -> kw = identifier) keywords
  || identifier.StartsWith "op_"
  || identifier.StartsWith "|" (* active patterns *)

let private caseCheck src style (identifier: string) (range: range) =
  match style with
  | LowerCamelCase ->
    if Char.IsLower identifier[0] || identifier[0] = '_' then ()
    else reportWarn src range $"'{identifier}' should be {LowerCamelCase}."
  | PascalCase ->
    if Char.IsUpper identifier[0] || identifier[0] = '_' then ()
    else reportWarn src range $"'{identifier}' should be {PascalCase}."

let private underscoreCheck src (identifier: string) (range: range) =
  let positionOfUnderscore = identifier[1..].IndexOf '_'
  if positionOfUnderscore = -1 then ()
  else reportWarn src range $"Remove underscore(s) in '{identifier}'"

let check src style checkUnderscore (identifier: string) (range: range) =
  if identifier.Contains " " || isKnownKeyWord identifier then ()
  else
    caseCheck src style identifier range
    if checkUnderscore then underscoreCheck src identifier range
    else ()