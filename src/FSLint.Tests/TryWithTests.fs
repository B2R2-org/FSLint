namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TryWithTests() =

  [<TestMethod>]
  member _.``[TryWith] Single case without bar - good``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with ex ->\n" +
    "    printfn \"error\"\n"
    |> lint

  [<TestMethod>]
  member _.``[TryWith] Single specific exception without bar - good``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with :? System.IO.IOException as ex ->\n" +
    "    printfn \"IO error\"\n"
    |> lint

  [<TestMethod>]
  member _.``[TryWith] Single wildcard without bar - good``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with _ ->\n" +
    "    printfn \"error\"\n"
    |> lint

  [<TestMethod>]
  member _.``[TryWith] Multiple cases with bars - good``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with\n" +
    "  | System.IO.IOException as ex -> printfn \"IO\"\n" +
    "  | System.TimeoutException as ex -> printfn \"Timeout\"\n" +
    "  | _ -> printfn \"Other\"\n"
    |> lint

  [<TestMethod>]
  member _.``[TryWith] Two cases with bars - good``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with\n" +
    "  | :? System.IO.IOException -> printfn \"IO\"\n" +
    "  | _ -> printfn \"Other\"\n"
    |> lint

  [<TestMethod>]
  member _.``[TryWith] Error - single case with bar``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with | ex ->\n" +
    "    printfn \"error\"\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[TryWith] Error - single specific exception with bar``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with\n" +
    "  | :? System.IO.IOException as ex ->\n" +
    "    printfn \"IO error\"\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[TryWith] Error - single wildcard with bar``() =
    "let test () =\n" +
    "  try\n" +
    "    riskyOp ()\n" +
    "  with | _ ->\n" +
    "    printfn \"error\"\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[TryWith] Nested try-with - good``() =
    "let test () =\n" +
    "  try\n" +
    "    try\n" +
    "      inner ()\n" +
    "    with ex1 -> ()\n" +
    "  with ex2 -> ()\n"
    |> lint

  [<TestMethod>]
  member _.``[TryWith] Try-with in expression - good``() =
    "let x = try compute () with ex -> 0\n" |> lint