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

  let goodRankTwoArrayTest =
    """
let fn (grid: int[,]) = 10
"""

  let goodRankThreeArrayTest =
    """
let fn (cube: int[,,]) = 10
"""

  let badRankTwoArraySpacingTest =
    """
let fn (grid: int [,]) = 10
"""

  let badRankTwoArrayInnerSpacingTest =
    """
let fn (grid: int[ ,]) = 10
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

  let goodParenTypeAnnotationTest =
    """
let f (x: (int -> int)) = x
"""

  let badParenTypeAnnotationTest =
    """
let f (x:(int -> int)) = x
"""

  let badParenTypeArrowSpacingTest =
    """
let f (x: (int->int)) = x
"""

  let goodAnonTypeAnnotationTest =
    """
let f (x: _) = x
"""

  let duplicateColonTest =
    """
type Class() =
  member _.Foo(sb:System.Text.StringBuilder) = None
"""

  let goodConstrainedTypeTest =
    """
let fn (opcode: 'Op when 'Op: enum<int>) = opcode
"""

  /// The colon is judged through the wrapper, in each of its three ways.
  let badConstrainedColonAfterTest =
    """
let fn (opcode:'Op when 'Op: enum<int>) = opcode
"""

  let badConstrainedColonBeforeTest =
    """
let fn (opcode :'Op when 'Op: enum<int>) = opcode
"""

  let badConstrainedColonSpacedTest =
    """
let fn (opcode : 'Op when 'Op: enum<int>) = opcode
"""

  let badConstrainedColonWideTest =
    """
let fn (opcode:  'Op when 'Op: enum<int>) = opcode
"""

  /// The constrained type keeps its own rules: a type application is still
  /// measured for its angle brackets on both sides.
  let goodConstrainedAppTest =
    """
let fn (xs: List<'T> when 'T: comparison) = xs
"""

  let badConstrainedAppOpenTest =
    """
let fn (xs: List< 'T> when 'T: comparison) = xs
"""

  let badConstrainedAppCloseTest =
    """
let fn (xs: List<'T > when 'T: comparison) = xs
"""

  let badConstrainedAppCommaTest =
    """
let fn (m: Map<'K,'V> when 'K: comparison) = m
"""

  /// An array, a tuple and a parenthesised type under a constraint likewise.
  let badConstrainedArrayTest =
    """
let fn (xs: 'T[] [] when 'T: comparison) = xs
"""

  let badConstrainedTupleTest =
    """
let fn (pair: 'T*'T when 'T: comparison) = pair
"""

  let badConstrainedParenTest =
    """
let fn (x: ( 'T) when 'T: comparison) = x
"""

  /// The clause is not about 'enum': every kind of constraint wraps the type
  /// the same way, and so does a chain of them.
  let goodConstraintKindsTest =
    """
let f1 (x: 'T when 'T: comparison) = x
let f2 (x: 'T when 'T: equality) = x
let f3 (x: 'T when 'T: null) = x
let f4 (x: 'T when 'T: unmanaged) = x
let f5 (x: 'T when 'T: struct) = x
let f6 (x: 'T when 'T :> System.IDisposable) = x
let f7 (x: 'T when 'T: comparison and 'T: equality) = x
"""

  let badConstraintKindComparisonTest =
    """
let fn (x:'T when 'T: comparison) = x
"""

  let badConstraintKindSubtypeTest =
    """
let fn (x:'T when 'T :> System.IDisposable) = x
"""

  let badConstraintKindChainTest =
    """
let fn (x:'T when 'T: comparison and 'T: equality) = x
"""

  /// The clause reaches every place an annotation can sit.
  let goodConstrainedPositionsTest =
    """
let fn (x: 'T when 'T: comparison) (y: 'U when 'U: equality) = x, y
let lam = fun (x: 'T when 'T: comparison) -> x
type Holder(x: 'T when 'T: comparison) =
  member _.Take(y: 'U when 'U: equality) = y
"""

  let badConstrainedSecondParamTest =
    """
let fn (x: 'T when 'T: comparison) (y:'U when 'U: equality) = x, y
"""

  let badConstrainedLambdaParamTest =
    """
let lam = fun (x:'T when 'T: comparison) -> x
"""

  let badConstrainedCtorParamTest =
    """
type Holder(x:'T when 'T: comparison) =
  member _.X = x
"""

  let badConstrainedMemberParamTest =
    """
type Holder() =
  member _.Take(y:'U when 'U: equality) = y
"""

  /// A constraint written on the binding's own type parameter list never took
  /// this path, and still does not.
  let goodTyparDeclConstraintTest =
    """
let fn<'T when 'T: comparison> (x: 'T) = x
"""

  let badTyparDeclConstraintTest =
    """
let fn<'T when 'T: comparison> (x:'T) = x
"""

  /// The clause itself carries no rule of its own, so spacing inside it is
  /// left alone. This pins the edge of what was added.
  let goodLooseSpacingInsideClauseTest =
    """
let fn (opcode: 'Op when 'Op: enum< int >) = opcode
"""

  [<TestMethod>]
  member _.``Type Annotation Empty Paren Test``() =
    lint goodEmptyParenTest
    lintAssert badEmptyParenTest

  [<TestMethod>]
  member _.``Type Annotation Extern Decl Test``() = lint goodExternDeclTest

  [<TestMethod>]
  member _.``Type Annotation Int Array Test``() =
    lint goodTypeAnnotationIntArrayTest
    lintAssert badTypeAnnotationIntArrayTest

  /// A rank is written with a comma for every dimension past the first, so a
  /// rank-two array closes one column further out than a rank-one array does,
  /// and a rank-three array two. Measuring the close against two rather than
  /// against the rank read every `int[,]` as an `int[ ]`.
  [<TestMethod>]
  member _.``Type Annotation Array Rank Test``() =
    lint goodRankTwoArrayTest
    lint goodRankThreeArrayTest
    lintAssert badRankTwoArraySpacingTest
    lintAssert badRankTwoArrayInnerSpacingTest

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

  [<TestMethod>]
  member _.``Type Annotation Paren And Anon Type Test``() =
    lint goodParenTypeAnnotationTest
    lint goodAnonTypeAnnotationTest
    lintAssert badParenTypeAnnotationTest
    lintAssert badParenTypeArrowSpacingTest

  [<TestMethod>]
  member _.``Type Annotation No Duplicate Colon Warning Test``() =
    let colonWarnings =
      lintErrors duplicateColonTest
      |> List.filter (fun e -> e.Message = "Use single whitespace after ':'")
    Assert.AreEqual<int>(1, colonWarnings.Length)

  /// The colon before a constrained type is judged in each of its three ways.
  [<TestMethod>]
  member _.``Type Annotation Constrained Colon Test``() =
    lint goodConstrainedTypeTest
    lintAssertMsg "Use single whitespace after ':'" badConstrainedColonAfterTest
    lintAssertMsg "Remove whitespace before ':'" badConstrainedColonBeforeTest
    lintAssertMsg "Use ': '" badConstrainedColonSpacedTest
    lintAssertMsg "Use single whitespace after ':'" badConstrainedColonWideTest

  /// The constrained type keeps the rules of its own shape.
  [<TestMethod>]
  member _.``Type Annotation Constrained Application Test``() =
    lint goodConstrainedAppTest
    lintAssertMsg "Remove whitespace after '<'" badConstrainedAppOpenTest
    lintAssertMsg "Remove whitespace before '>'" badConstrainedAppCloseTest
    lintAssertMsg "Use single whitespace after ','" badConstrainedAppCommaTest

  [<TestMethod>]
  member _.``Type Annotation Constrained Shape Test``() =
    lintAssertMsg "Remove whitespace around '[]'" badConstrainedArrayTest
    lintAssertMsg "Use ' * '" badConstrainedTupleTest
    lintAssertMsg "Remove whitespace after '('" badConstrainedParenTest

  /// Every kind of constraint wraps the type the same way, 'enum' included.
  [<TestMethod>]
  member _.``Type Annotation Constraint Kinds Test``() =
    lint goodConstraintKindsTest
    lintAssertMsg "Use single whitespace after ':'"
      badConstraintKindComparisonTest
    lintAssertMsg "Use single whitespace after ':'" badConstraintKindSubtypeTest
    lintAssertMsg "Use single whitespace after ':'" badConstraintKindChainTest

  /// The clause reaches every place an annotation can sit.
  [<TestMethod>]
  member _.``Type Annotation Constrained Positions Test``() =
    lint goodConstrainedPositionsTest
    lintAssertMsg "Use single whitespace after ':'"
      badConstrainedSecondParamTest
    lintAssertMsg "Use single whitespace after ':'"
      badConstrainedLambdaParamTest

  [<TestMethod>]
  member _.``Type Annotation Constrained Positions Test(2)``() =
    lintAssertMsg "Use single whitespace after ':'" badConstrainedCtorParamTest
    lintAssertMsg "Use single whitespace after ':'"
      badConstrainedMemberParamTest

  /// A constraint on the binding's own type parameter list is a separate path,
  /// and was working before.
  [<TestMethod>]
  member _.``Type Annotation Typar Declaration Constraint Test``() =
    lint goodTyparDeclConstraintTest
    lintAssertMsg "Use single whitespace after ':'" badTyparDeclConstraintTest

  /// Spacing inside the clause carries no rule of its own, and the annotation
  /// ahead of it is still judged. Both halves are pinned here.
  [<TestMethod>]
  member _.``Type Annotation Constraint Clause Untouched Test``() =
    lint goodLooseSpacingInsideClauseTest
    lintErrors badConstrainedColonAfterTest
    |> List.filter (fun e -> e.Message = "Use single whitespace after ':'")
    |> fun errors -> Assert.AreEqual<int>(1, errors.Length)

  /// A union case field is asked the gap between its name and its type here.
  /// A record field is asked it where the rest of a record is read, and this
  /// is the only place a union case gets the question.
  [<TestMethod>]
  member _.``[TypeAnnotation] Union Case Field Colon Test``() =
    lint "type T =\n  | X of a: int * b: int\n"
    lintAssertMsg "Use ': '" "type T =\n  | X of a:int * b: int\n"
