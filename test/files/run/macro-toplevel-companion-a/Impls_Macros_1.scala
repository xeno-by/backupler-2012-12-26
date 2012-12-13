import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val synthetic = reify{ class C { override def toString = "C" }; object C { implicit val c = new C } }.tree
    if (!c.existsAmongTrees(TypeName("C"))) c.introduceTopLevel(synthetic)
    Ident(TypeName("C"))
    // if (!c.existsAmongTrees(TypeName("test.C"))) c.introduceTopLevel(PackageDef(Ident(TermName("test")), synthetic.asInstanceOf[Block].stats))
    // Select(Ident(TermName("test")), TypeName("C"))
  }

  type Foo = macro impl
}