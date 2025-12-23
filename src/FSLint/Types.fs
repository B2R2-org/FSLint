namespace B2R2.FSLint

open FSharp.Compiler.Text

type CaseStyle =
  | LowerCamelCase
  | PascalCase

type MemberCategory =
  | ConstantField = 0
  | Field = 1
  | Constructor = 2
  | Event = 3
  | Property = 4
  | Indexer = 5
  | Method = 6
  | NestedType = 7

type AccessLevel =
  | Public = 0
  | Internal = 1
  | Protected = 2
  | Private = 3

type MemberScope =
  | Static = 0
  | Instance = 1

type AccessModifierLevel =
  | Public
  | Private

and ScopeContext =
  { ModuleAccess: AccessModifierLevel
    TypeAccess: AccessModifierLevel option }

type CheckContext =
  { ModuleAccess: AccessModifierLevel }

type LintError =
  { Range: range
    Message: string
    LineContent: string
    ColumnIndicator: string }

type LintContext =
  { mutable Errors: LintError list
    Source: ISourceText
    FilePath: string }

type LintOutcome =
  { Index: int
    Path: string
    Ok: bool
    Log: string
    Errors: LintError list }