import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    UnApply(Ident(newTypeName("C")), List(x.tree))
  }
}
