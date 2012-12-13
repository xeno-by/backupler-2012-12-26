import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val cdef = reify{ class C }.tree
    if (!c.existsAmongTrees(TypeName("C"))) c.introduceTopLevel(cdef)
    val mdef = reify{ object C }.tree
    if (!c.existsAmongTrees(TermName("C"))) c.introduceTopLevel(mdef)
    Ident(TypeName("C"))
  }

  type Foo = macro impl
}