[<AutoOpen>]
module Lint

open B2R2.FSLint
open B2R2.FSLint.Program
open Microsoft.VisualStudio.TestTools.UnitTesting

let lint context =
  isStrict <- true
  linterForFs.Lint(FakeFsPath, context)

let lintAssert context =
  isStrict <- true
  Assert.ThrowsException<LintException>(fun () ->
    linterForFs.Lint(FakeFsPath, context)) |> ignore