import scala.reflect.macros.Context

object Impls {
  def tm(c: Context)() = c.universe.Ident(c.universe.newTypeName("C"))
  def tm_a(c: Context)() = c.universe.Ident(c.universe.newTypeName("A"))
  def tm_l(c: Context)() = c.universe.Ident(c.universe.newTypeName("List"))
  def tm_t(c: Context)() = c.universe.Ident(c.universe.newTypeName("T"))
  def tm_d(c: Context)() = c.universe.Ident(c.universe.newTypeName("D"))
}

object Macros {
  type TM() = macro Impls.tm
  type TM_A() = macro Impls.tm_a
  type TM_L() = macro Impls.tm_l
  type TM_T() = macro Impls.tm_t
  type TM_D() = macro Impls.tm_d
}

class A extends scala.annotation.StaticAnnotation
class B
trait C
class D
