[<AutoOpen>]
module Lint

open B2R2.FSLint
open B2R2.FSLint.Program
open Microsoft.VisualStudio.TestTools.UnitTesting

let lint context =
  setStrictMode ()
  linterForFs.Lint(FakeFsPath, context)

let lintAssert context =
  setStrictMode ()
  Assert.ThrowsException<LintException>(fun () ->
    linterForFs.Lint(FakeFsPath, context)) |> ignore