namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ClassMemberOrderTests() =

  [<TestMethod>]
  member _.``[MemberOrder] Properties before methods - good``() =
    "type MyClass() =\n" +
    "  member _.Property = 42\n" +
    "  member _.Method() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Static before instance - good``() =
    "type MyClass() =\n" +
    "  static member StaticMethod() = ()\n" +
    "  member _.InstanceMethod() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Complete order - good``() =
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
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Multiple properties then methods - good``() =
    "type MyClass() =\n" +
    "  member _.Prop1 = 1\n" +
    "  member _.Prop2 = 2\n" +
    "  member _.Prop3 = 3\n" +
    "\n" +
    "  member _.Method1() = ()\n" +
    "  member _.Method2() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Auto-properties before methods - good``() =
    "type MyClass() =\n" +
    "  member val AutoProp = 42 with get, set\n" +
    "\n" +
    "  member _.Method() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Error - method before property``() =
    "type MyClass() =\n" +
    "  member _.Method() = ()\n" +
    "  member _.Property = 42\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[MemberOrder] Error - instance before static``() =
    "type MyClass() =\n" +
    "  member _.InstanceMethod() = ()\n" +
    "  static member StaticMethod() = ()\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[MemberOrder] Error - method before field``() =
    "type MyClass() =\n" +
    "  member _.Method() = ()\n" +
    "  val mutable Field: int\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[MemberOrder] Error - static instance before static static``() =
    "type MyClass() =\n" +
    "  member _.InstanceProp = 1\n" +
    "  static member StaticProp = 2\n"
    |> lintAssert

  [<TestMethod>]
  member _.``[MemberOrder] Interface implementation ignored - good``() =
    "type MyClass() =\n" +
    "  member _.Method() = ()\n" +
    "\n" +
    "  interface System.IDisposable with\n" +
    "    member _.Dispose() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Abstract members - good``() =
    "[<AbstractClass>]\n" +
    "type MyClass() =\n" +
    "  abstract member AbstractProp: int\n" +
    "  abstract member AbstractMethod: unit -> unit\n"
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Single member - good``() =
    "type MyClass() =\n" +
    "  member _.OnlyMethod() = ()\n"
    |> lint

  [<TestMethod>]
  member _.``[MemberOrder] Operators treated as methods - good``() =
    "type MyClass() =\n" +
    "  member _.Property = 42\n" +
    "\n" +
    "  static member (+) (a: MyClass, b: MyClass) = MyClass()\n"
    |> lint