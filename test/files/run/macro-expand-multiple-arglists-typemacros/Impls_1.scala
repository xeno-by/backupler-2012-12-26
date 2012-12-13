import scala.reflect.macros.Context

class C
object Impls {
  def impl(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = {
    import c.universe._
    // Apply(Apply(Ident(newTypeName("C")), List(x.tree)), List(y.tree))
    Ident(newTypeName("C"))
  }
}