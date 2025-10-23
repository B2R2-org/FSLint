namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open B2R2.FSLint
open B2R2.FSLint.Program

[<TestClass>]
type IfThenElseTests() =

  [<TestMethod>]
  member _.``[IfThenElse] Case 1 - both fit correctly``() =
    let code =
      """
let x =
  if cond then expr
  else alternative
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Case 1 - short names``() =
    let code =
      """
let result =
  if a then b
  else c
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Case 1 - with function calls``() =
    let code =
      """
let x =
  if check () then getValue ()
  else getDefault ()
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Case 2 - if-then-expr exceeds``() =
    let code =
      """
let x =
  if veryLongConditionNameThatDefinitelyMakesThisLineExceedEightyColumns then
    result
  else
    alternative
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Case 2 - else-expr exceeds``() =
    let code =
      """
let x =
  if condition then
    expr
  else
    veryLongAlternativeExpressionNameThatDefinitelyExceedsEightyColumnsForSure
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Case 2 - both exceed``() =
    let code =
      """
let x =
  if veryLongConditionNameThatExceedsEightyColumnsDefinitelyAndAbsolutely then
    veryLongThenExpressionNameThatAlsoDefinitelyExceedsEightyColumnsForSure
  else
    veryLongElseAlternativeExpressionNameThatDefinitelyExceedsEightyColumns
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Case 3 - multiline condition``() =
    let code =
      """
let x =
  if cond1 &&
     cond2 &&
     cond3
  then expr
  else alt
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Exception - match in condition``() =
    let code =
      """
let x =
  if (match opt with
      | Some _ -> true
      | None -> false) then
    result
  else
    alternative
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Exception - match in then expr``() =
    let code =
      """
let x =
  if cond then
    match value with
    | A -> 1
    | B -> 2
  else
    alternative
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Exception - match in else expr``() =
    let code =
      """
let x =
  if cond then
    result
  else
    match value with
    | A -> 1
    | B -> 2
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Error - Case 1 but over-separated``() =
    let code =
      """
let x =
  if cond then
    expr
  else
    alt
"""
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Error - Case 2 but all on one line``() =
    let code =
      """let myResult =
      if veryLongConditionNameThatDefinitely then valueExpression
      else otherExpression
      """
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Error - Case 2 but expr not separated``() =
    let code =
      """let myVariable =
      if veryLongConditionNameThatDefinitelyandSomewhat then resultExpression
      else alternativeExpression
      """
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] Error - Case 2 but partial separation``() =
    let code =
      """let myLongVariableName =
      if veryLongConditionNameThatDefinitely then resultExpression
      else alternativeExpression
      """
    Assert.ThrowsException<LintException>(fun () ->
      linterForFs.Lint(Constants.FakeFsPath, code)
    ) |> ignore

  [<TestMethod>]
  member _.``[IfThenElse] If without else - no check``() =
    let code =
      """
let check x =
  if x > 0 then printfn "positive"
"""
    linterForFs.Lint(Constants.FakeFsPath, code)

  [<TestMethod>]
  member _.``[IfThenElse] Nested if-then-else``() =
    let code =
      """
let x =
  if outer then inner
  else c
"""
    linterForFs.Lint(Constants.FakeFsPath, code)