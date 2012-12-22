import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = {
    import c.universe._
    println(c.freshName())
    println(c.freshName("qwe"))
    println(c.freshName(TypeName("qwe")))
    c.abort(NoPosition, "blargh")
  }
}

object Macros {
  def foo = macro Impls.impl
}