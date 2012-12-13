object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.tools.reflect.ToolBox
  val ListOfNil = List(List())
  val foo = Select(Ident(newTermName("Macros")), newTypeName("Foo"))
  val ctor = DefDef(NoMods, nme.CONSTRUCTOR, Nil, ListOfNil, TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
  val valdef = ValDef(NoMods, newTermName("foo"), foo.duplicate, Literal(Constant(null)))
  val classDef = ClassDef(NoMods, newTypeName("C"), Nil, Template(List(foo.duplicate), emptyValDef, List(ctor, valdef)))
  try cm.mkToolBox().eval(Block(List(classDef), Literal(Constant(()))))
  catch { case ex: Throwable =>  println(ex.getMessage) }
}