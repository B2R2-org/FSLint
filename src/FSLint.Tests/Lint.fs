[<AutoOpen>]
module Lint

open B2R2.FSLint
open B2R2.FSLint.Program
open FSharp.Compiler.Text
open Microsoft.VisualStudio.TestTools.UnitTesting

let lint context =
  isStrict <- true
  linterForFs.Lint(FakeFsPath, context)

let lintAssert context =
  isStrict <- true
  Assert.ThrowsException<LintException>(fun () ->
    linterForFs.Lint(FakeFsPath, context)) |> ignore

let lintAssertMsg (expected: string) context =
  isStrict <- true
  let ex =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, context))
  match ex :> exn with
  | LintException msg -> StringAssert.Contains(msg, expected)
  | _ -> Assert.Fail "Expected LintException"

let lintErrors (source: string) =
  isStrict <- true
  let context =
    { Errors = []
      Source = SourceText.ofString source
      FilePath = FakeFsPath
      EditorConfig = Configuration.defaultSettings }
  linterForFsWithContext(Some context).Lint(FakeFsPath, source)
  List.rev context.Errors