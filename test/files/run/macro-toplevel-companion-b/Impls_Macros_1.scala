import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val Block(List(cdef: ClassDef), _) = reify{ class C }.tree
    if (!c.existsAmongTrees(newTypeName("C"))) c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, cdef)
    val Block(List(mdef: ModuleDef), _) = reify{ object C }.tree
    if (!c.existsAmongTrees(newTypeName("C"))) c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, mdef)
    c.literalUnit
  }

  def foo = macro impl
}