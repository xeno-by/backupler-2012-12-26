import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Ctx)(x: c.Expr[T]) = {
    import c.universe._
    val name = TypeName("C_" + c.weakTypeOf[T].toString + "_" + c.weakTypeOf[U].toString + "_" + x.tree.toString.replace("\"", "quote"))
    if (!c.existsAmongTrees(name)) {
      val Block(List(dummy: ClassDef), _) = reify{ class DUMMY }.tree
      val synthetic = ClassDef(NoMods, name, Nil, dummy.impl)
      c.introduceTopLevel(synthetic)
    }
    Ident(name)
  }
}