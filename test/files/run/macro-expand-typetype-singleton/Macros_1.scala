import scala.reflect.macros.{Context => Ctx}

class C
object Macros {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    SingletonTypeTree(Ident(newTermName("c")))
  }

  type Foo(x: Int) = macro foo
}
