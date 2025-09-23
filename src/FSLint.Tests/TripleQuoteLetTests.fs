namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program
open B2R2.FSLint.Tests

[<TestClass>]
type TripleQuoteLetTests() =

  let badSameLineSimple =
    "let f () =\n" +
    "  let a = \"\"\"\n" +
    "  hello\n" +
    "  \"\"\"\n" +
    "  a\n"

  let badSameLineLetRec =
    "let f () =\n" +
    "  let  rec  parse = \"\"\"\n" +
    "  text\n" +
    "  \"\"\"\n" +
    "  parse\n"

  let goodNextLine =
    "let f () =\n" +
    "  let a =\n" +
    "    \"\"\"\n" +
    "    hello\n" +
    "    \"\"\"\n" +
    "  a\n"

  let goodNormalStringSameLine =
    "let f () =\n" +
    "  let a = \"hello\"\n" +
    "  a\n"

  let goodSeparatedBinding =
    "let f () =\n" +
    "  let x = 42\n" +
    "  let y =\n" +
    "    \"\"\"\n" +
    "    multiline\n" +
    "    \"\"\"\n" +
    "  x + y.Length\n"

  [<TestMethod>]
  member _.``[TripleQuote] Detect same-line let and triple quote (simple)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badSameLineSimple)
    ) |> ignore

  [<TestMethod>]
  member _.``[TripleQuote] Detect same-line let and triple quote (let rec)``() =
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, badSameLineLetRec)
    ) |> ignore

  [<TestMethod>]
  member _.``[TripleQuote] Allow next-line triple quote after let``() =
    linterForFs.Lint(Constants.FakeFsPath, goodNextLine)

  [<TestMethod>]
  member _.``[TripleQuote] Allow normal string on same line``() =
    linterForFs.Lint(Constants.FakeFsPath, goodNormalStringSameLine)

  [<TestMethod>]
  member _.``[TripleQuote] Allow triple quote when not on the let line``() =
    linterForFs.Lint(Constants.FakeFsPath, goodSeparatedBinding)
