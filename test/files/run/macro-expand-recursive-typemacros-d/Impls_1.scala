import scala.reflect.macros.{Context => Ctx}

class C(x: Int)
object Impls {
  def impl(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    Apply(Ident(newTypeName("C")), List(x.tree))
  }

  def implImpl(c: Ctx) = {
    import c.universe._
    Apply(Select(Ident(newTermName("Macros")), newTypeName("Foo")), List(Literal(Constant(2))))
  }
}