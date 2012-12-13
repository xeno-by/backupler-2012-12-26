import scala.reflect.macros.{Context => Ctx}

class C
object Macros {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    import Flag._
    val trivialTypeBounds = TypeBoundsTree(Ident(newTypeName("Nothing")), Ident(newTypeName("Any")))
    ExistentialTypeTree(Ident(newTypeName("T")), List(TypeDef(Modifiers(DEFERRED), newTypeName("T"), Nil, trivialTypeBounds)))
  }

  type Foo(x: Int) = macro foo
}
