import scala.reflect.macros.Context

class B
class C extends B

object Impls {
  def impl_b(c: Context) = c.universe.Ident(c.universe.newTypeName("B"))
  def impl_c(c: Context) = c.universe.Ident(c.universe.newTypeName("C"))
}
