import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = c.universe.Ident(c.universe.newTypeName("IDoNotExist"))
}

object Macros {
  type Foo = macro Impls.foo
}