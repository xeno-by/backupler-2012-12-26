import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[T]) = {
    import c.universe._
    val name = newTypeName("C_" + c.weakTypeOf[T].toString + "_" + x.tree.toString)
    if (!c.existsAmongTrees(name)) {
      val Block(List(dummy: ClassDef), _) = reify{ class DUMMY }.tree
      val synthetic = ClassDef(NoMods, name, Nil, dummy.impl)
      c.introduceTopLevel(synthetic)
    }
    Ident(name)
  }
}