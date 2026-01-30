namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type ClassMemberOrderTests() =

  [<TestMethod>]
  member _.``[MemberOrder] Properties before methods - good``() =
    let code =
      "type MyClass() =\n" +
      "  member _.Property = 42\n" +
      "  member _.Method() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Static before instance - good``() =
    let code =
      "type MyClass() =\n" +
      "  static member StaticMethod() = ()\n" +
      "  member _.InstanceMethod() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Complete order - good``() =
    let code =
      "type MyClass() =\n" +
      "  val mutable private _field: int\n" +
      "\n" +
      "  static member StaticProp = 10\n" +
      "\n" +
      "  member _.InstanceProp = 20\n" +
      "\n" +
      "  static member StaticMethod() = ()\n" +
      "\n" +
      "  member _.InstanceMethod() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Multiple properties then methods - good``() =
    let code =
      "type MyClass() =\n" +
      "  member _.Prop1 = 1\n" +
      "  member _.Prop2 = 2\n" +
      "  member _.Prop3 = 3\n" +
      "\n" +
      "  member _.Method1() = ()\n" +
      "  member _.Method2() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Auto-properties before methods - good``() =
    let code =
      "type MyClass() =\n" +
      "  member val AutoProp = 42 with get, set\n" +
      "\n" +
      "  member _.Method() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Error - method before property``() =
    let code =
      "type MyClass() =\n" +
      "  member _.Method() = ()\n" +
      "  member _.Property = 42\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[MemberOrder] Error - instance before static``() =
    let code =
      "type MyClass() =\n" +
      "  member _.InstanceMethod() = ()\n" +
      "  static member StaticMethod() = ()\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[MemberOrder] Error - method before field``() =
    let code =
      "type MyClass() =\n" +
      "  member _.Method() = ()\n" +
      "  val mutable Field: int\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[MemberOrder] Error - static instance before static static``() =
    let code =
      "type MyClass() =\n" +
      "  member _.InstanceProp = 1\n" +
      "  static member StaticProp = 2\n"
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[MemberOrder] Interface implementation ignored - good``() =
    let code =
      "type MyClass() =\n" +
      "  member _.Method() = ()\n" +
      "\n" +
      "  interface System.IDisposable with\n" +
      "    member _.Dispose() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Abstract members - good``() =
    let code =
      "[<AbstractClass>]\n" +
      "type MyClass() =\n" +
      "  abstract member AbstractProp: int\n" +
      "  abstract member AbstractMethod: unit -> unit\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Single member - good``() =
    let code =
      "type MyClass() =\n" +
      "  member _.OnlyMethod() = ()\n"
    linterForFs.Lint(FakeFsPath, code)

  [<TestMethod>]
  member _.``[MemberOrder] Operators treated as methods - good``() =
    let code =
      "type MyClass() =\n" +
      "  member _.Property = 42\n" +
      "\n" +
      "  static member (+) (a: MyClass, b: MyClass) = MyClass()\n"
    linterForFs.Lint(FakeFsPath, code)