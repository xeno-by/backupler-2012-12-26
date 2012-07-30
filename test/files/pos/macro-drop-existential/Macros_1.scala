import scala.reflect.makro.Context

trait Data[T] {
  def value: T = macro Def.valueMacroImpl[T]
}

object Wrap {
  def wrap[T](a: Any): T = ???
}

object Def {
  def valueMacroImpl[T](c: Context)(t0: c.TypeTag[T]): c.Expr[T] =
    c.macroApplication match {
      case c.universe.Select(t, _) =>
        val ts = c.Expr[Any](t)
        implicit val t1 = c.TypeTag[T](c.dropExistential(t0.tpe))
        //implicit val t1 = t0
        c.universe.reify { Wrap.wrap[T](ts.splice) }
    }
}