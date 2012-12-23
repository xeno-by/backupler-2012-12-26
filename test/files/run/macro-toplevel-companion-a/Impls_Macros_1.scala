import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val synthetic = reify{ class C { override def toString = "C" }; object C { implicit val c = new C } }.tree
    val defs = synthetic.asInstanceOf[Block].stats.asInstanceOf[List[ImplDef]]
    if (!c.existsAmongTrees(TypeName("C"))) c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, defs: _*)
    Ident(TypeName("C"))
    // if (!c.existsAmongTrees(TypeName("test.C"))) c.introduceTopLevel("test", defs))
    // Select(Ident(TermName("test")), TypeName("C"))
  }

  type Foo = macro impl
}