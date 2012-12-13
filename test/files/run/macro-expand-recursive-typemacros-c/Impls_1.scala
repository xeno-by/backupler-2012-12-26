import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def impl(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    Ident(newTypeName("C"))
  }

  def implImpl(c: Ctx) = {
    import c.universe._
    Apply(Select(Ident(newTermName("Macros")), newTypeName("Foo")), List(Literal(Constant(2))))
  }
}