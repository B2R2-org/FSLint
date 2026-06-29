namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TypeAnnotationTest() =
  let goodEmptyParenTest =
    """
let fn () = ()
"""

  let badEmptyParenTest =
    """
let fn ( ) = ()
"""

  let goodTypeAnnotationIntArrayTest =
    """
let fn (param: int[]) = 10
"""

  let badTypeAnnotationIntArrayTest =
    """
let fn (param: int []) = 10
"""

  let goodTypeAnnotationIntTest =
    """
let fn (p: int) = 10
"""

  let badTypeAnnotationIntTest =
    """
let fn (p:int) = 10
"""

  let goodTypeAnnotationStringTest =
    """
let fn (str: string) = "10"
"""

  let badTypeAnnotationStringTest =
    """
let fn (str:string) = "10"
"""

  let goodTypeAnnotationStringSpaceTest =
    """
let fn (str: string) = "10"
"""

  let badTypeAnnotationStringSpaceTest =
    """
let fn (str :string) = "10"
"""

  let goodTypeAnnotationArrayTest =
    """
let inline toString (stmts: LowUIR.Stmt[]) = ()
"""

  let badTypeAnnotationArrayTest =
    """
let inline toString (stmts:  LowUIR.Stmt[]) = ()
"""

  let goodTypeAnnotationColonSpacingTest =
    """
type X = X of a: int * b: int
"""

  let badTypeAnnotationColonSpacingTest =
    """
type X = X of a : int * b: int
"""

  let goodTypeAnnotationStarSpacingTest =
    """
type X = X of a: int * b: int
"""

  let badTypeAnnotationStarSpacingTest =
    """
type X = X of a : int* b: int
"""

  let goodTypeAnnotationArraySpacingTest =
    """
type X = X of a: int[]
"""

  let badTypeAnnotationArraySpacingTest =
    """
type X = X of a: int []
"""

  let goodParamArrayTest =
    """
let x (x: int[]) = x
"""

  let badParamArrayTest =
    """
let x (x: int []) = x
"""

  let goodTupleArrayTest =
    """
let x (x: int[] * int) = x
"""

  let badTupleArrayTest =
    """
let x (x: int[] * int []) = x
"""

  let goodArrowSpacingTest =
    """
let fn (x: list<int> -> string[] * string) = x
"""

  let badArrowSpacingTest =
    """
let fn (x: list<int>->string[] * string) = x
"""

  let goodAnonRecdTest =
    """
let good (item: {| Addr: Addr
                   Name: string
                   ELFSectionHeader: ELF.SectionHeader option |}) = ()
"""

  let badAnonRecdTest =
    """
let bad (item: {| Addr:Addr
                  Name:string
                  ELFSectionHeader:ELF.SectionHeader option |}) = ()
"""

  let badAnonRecdLeftBracketSpacingTest =
    """
let bad (item: {|Addr: Addr; Name: string |}) = ()
"""

  let badAnonRecdRightBracketSpacingTest =
    """
let bad (item: {| Addr: Addr; Name: string|}) = ()
"""

  let badAnonRecdInnerTypeTest =
    """
let bad (item: {| Addr: Addr [] |}) = ()
"""

  let goodAbstractAnonRecdTest =
    """
type ITokenContextProvider =
  abstract GetInstructionInfo:
    Addr
    -> {| Stmts: string[]
          ReadAddrs: string[]
          PCTargets: Addr[] |}
"""

  let badAbstractAnonRecdLeftBracketTest =
    """
type ITokenContextProvider =
  abstract GetInstructionInfo:
    Addr
    -> {|Stmts: string[]
         ReadAddrs: string[]
         PCTargets: Addr[] |}
"""

  let badAbstractAnonRecdRightBracketTest =
    """
type ITokenContextProvider =
  abstract GetInstructionInfo:
    Addr
    -> {| Stmts: string[]
          ReadAddrs: string[]
          PCTargets: Addr[]|}
"""

  /// Regression for #155: an `extern` declaration has a fully synthetic body
  /// (`failwith ...` plus a return-type `SynType.App` with empty type args).
  /// The linter must not crash on it, and must not flag its synthetic body
  /// (e.g. with a spurious func-application spacing warning).
  let goodExternDeclTest =
    """
[<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
extern int private cRead(int fd, byte[] buf, int count)
"""

  let goodQualifiedTypeAnnotationTest =
    """
type Class() =
  member _.Foo(sb: System.Text.StringBuilder) = None
"""

  let badQualifiedTypeAnnotationTest =
    """
type Class() =
  member _.Foo(sb:System.Text.StringBuilder) = None
"""

  let goodQualifiedTypeFunctionTest =
    """
let f (sb: System.Text.StringBuilder) = sb
"""

  let goodQualifiedTypeTupleTest =
    """
let f (a: System.Text.StringBuilder, b: int) = a
"""

  let goodQualifiedTypeGenericTest =
    """
let f (m: System.Collections.Generic.List<int>) = m
"""

  let badQualifiedTypeSpaceBeforeTest =
    """
type Class() =
  member _.Foo(sb : System.Text.StringBuilder) = None
"""

  [<TestMethod>]
  member _.``Type Annotation Empty Paren Test``() =
    lint goodEmptyParenTest
    lintAssert badEmptyParenTest

  [<TestMethod>]
  member _.``Type Annotation Extern Decl Test``() =
    lint goodExternDeclTest

  [<TestMethod>]
  member _.``Type Annotation Int Array Test``() =
    lint goodTypeAnnotationIntArrayTest
    lintAssert badTypeAnnotationIntArrayTest

  [<TestMethod>]
  member _.``Type Annotation(int) Test``() =
    lint goodTypeAnnotationIntTest
    lintAssert badTypeAnnotationIntTest

  [<TestMethod>]
  member _.``Type Annotation(string) Test``() =
    lint goodTypeAnnotationStringTest
    lintAssert badTypeAnnotationStringTest

  [<TestMethod>]
  member _.``Type Annotation(string) Space Test``() =
    lint goodTypeAnnotationStringSpaceTest
    lintAssert badTypeAnnotationStringSpaceTest

  [<TestMethod>]
  member _.``Type Annotation(Array) Test``() =
    lint goodTypeAnnotationArrayTest
    lintAssert badTypeAnnotationArrayTest

  [<TestMethod>]
  member _.``Type Annotation Colon Space Test``() =
    lint goodTypeAnnotationColonSpacingTest
    lintAssert badTypeAnnotationColonSpacingTest

  [<TestMethod>]
  member _.``Type Annotation Star Space Test``() =
    lint goodTypeAnnotationStarSpacingTest
    lintAssert badTypeAnnotationStarSpacingTest

  [<TestMethod>]
  member _.``Type Annotation Array Space Test``() =
    lint goodTypeAnnotationArraySpacingTest
    lintAssert badTypeAnnotationArraySpacingTest

  [<TestMethod>]
  member _.``Type Annotation Param Array Space Test``() =
    lint goodParamArrayTest
    lintAssert badParamArrayTest

  [<TestMethod>]
  member _.``Type Annotation Tuple Array Space Test``() =
    lint goodTupleArrayTest
    lintAssert badTupleArrayTest

  [<TestMethod>]
  member _.``Type Annotation Arrow Space Test``() =
    lint goodArrowSpacingTest
    lintAssert badArrowSpacingTest

  [<TestMethod>]
  member _.``Type Annotation AnonRecd Colon Space Test``() =
    lint goodAnonRecdTest
    lintAssert badAnonRecdTest
    lintAssert badAnonRecdLeftBracketSpacingTest
    lintAssert badAnonRecdRightBracketSpacingTest
    lintAssert badAnonRecdInnerTypeTest

  [<TestMethod>]
  member _.``Type Annotation Abstract AnonRecd Bracket Space Test``() =
    lint goodAbstractAnonRecdTest
    lintAssert badAbstractAnonRecdLeftBracketTest
    lintAssert badAbstractAnonRecdRightBracketTest

  [<TestMethod>]
  member _.``Type Annotation Qualified Type Test``() =
    lint goodQualifiedTypeAnnotationTest
    lint goodQualifiedTypeFunctionTest
    lint goodQualifiedTypeTupleTest
    lint goodQualifiedTypeGenericTest
    lintAssert badQualifiedTypeAnnotationTest
    lintAssert badQualifiedTypeSpaceBeforeTest
