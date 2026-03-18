namespace B2R2.FSLint.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type DotGetTests() =

  [<TestMethod>]
  member _.``[DotGet] Simple property access``() =
    """let x = obj.Property""" |> lint

  [<TestMethod>]
  member _.``[DotGet] Chained property access``() =
    """let x = obj.Property.SubProperty.Value""" |> lint

  [<TestMethod>]
  member _.``[DotGet] In array literal``() =
    """let arr = [ obj.Prop1; obj.Prop2; obj.Prop3 ]""" |> lint

  [<TestMethod>]
  member _.``[DotGet] In tuple``() =
    """let t = (obj1.Prop, obj2.Prop, obj3.Prop)""" |> lint

  [<TestMethod>]
  member _.``[DotGet] Static member access``() =
    let code =
      """
let x = System.String.Empty

let y = System.Console.WriteLine
"""
    lint code

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (simple)``() =
    """let x = obj .Property""" |> lintAssert

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (chained)``() =
    """let x = obj.Property .SubProperty""" |> lintAssert

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (in array)``() =
    """let arr = [ obj .Prop; other.Prop ]""" |> lintAssert

  [<TestMethod>]
  member _.``[DotGet] Error - space before dot (in tuple)``() =
    """let t = (obj .Prop1, obj.Prop2)""" |> lintAssert

  [<TestMethod>]
  member _.``[DotGet] Error - space after dot (simple)``() =
    """let x = obj. Property""" |> lintAssert

  [<TestMethod>]
  member _.``[DotGet] Error - space after dot (chained)``() =
    """let x = obj.Property. SubProperty""" |> lintAssert

  [<TestMethod>]
  member _.``[DotGet] Error - space after dot (in array)``() =
    """let lst = [ obj. Prop; other.Prop ]""" |> lintAssert

  [<TestMethod>]
  member _.``[DotGet] Error - spaces on both sides``() =
    """let x = obj . Property""" |> lintAssert