import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val synthetic = reify{ class C { override def toString = "C" }; object C { implicit val c = new C } }.tree
    if (!c.existsTopLevel(newTypeName("C"))) c.introduceTopLevel(synthetic)
    Ident(newTypeName("C"))
    // if (!c.existsTopLevel(newTypeName("test.C"))) c.introduceTopLevel(PackageDef(Ident(newTermName("test")), synthetic.asInstanceOf[Block].stats))
    // Select(Ident(newTermName("test")), newTypeName("C"))
  }

  type Foo = macro impl
}