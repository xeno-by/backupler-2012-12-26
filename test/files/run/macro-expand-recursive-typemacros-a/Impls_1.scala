import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def impl(c: Ctx) = {
    import c.universe._
    Ident(newTypeName("C"))
  }

  def implImpl(c: Ctx) = {
    import c.universe._
    Select(Ident(newTermName("Macros")), newTypeName("Foo"))
  }
}