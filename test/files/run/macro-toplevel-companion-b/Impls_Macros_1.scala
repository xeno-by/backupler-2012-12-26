import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val cdef = reify{ class C }.tree
    if (!c.existsAmongTrees(newTypeName("C"))) c.introduceTopLevel(cdef)
    val mdef = reify{ object C }.tree
    if (!c.existsAmongTrees(newTermName("C"))) c.introduceTopLevel(mdef)
    Ident(newTypeName("C"))
  }

  type Foo = macro impl
}