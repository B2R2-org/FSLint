[<AutoOpenAttribute>]
module B2R2.FSLint.Tests.Utils

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint.Program

let assertFSLintSuccess path text =
  Assert.IsTrue(linterForFs.Lint(path, text).IsSuccess)

let assertFSLintFailure path text =
  Assert.IsTrue(linterForFs.Lint(path, text).IsFailure)
