import scala.reflect.macros.Context

class B
class C extends B

object Impls {
  def impl(c: Context) = c.universe.Ident(c.universe.newTypeName("C"))
  def impl1(c: Context)() = c.universe.Ident(c.universe.newTypeName("C"))
}
