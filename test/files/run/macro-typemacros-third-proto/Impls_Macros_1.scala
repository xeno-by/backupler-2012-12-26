import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    c.enclosingTemplate match {
      case Template(parents, self, defs) =>
        val parents1 = parents filter (_ != c.macroApplication)
        val defs1 = defs collect {
          case ddef @ DefDef(mods, name, tparams, vparamss, tpt, body) if name != nme.CONSTRUCTOR =>
            val future = Select(Select(Select(Ident(newTermName("scala")), newTermName("concurrent")), newTermName("package")), newTermName("future"))
            val body1 = Apply(future, List(body))
            DefDef(mods, newTermName("async" + name.toString.capitalize), tparams, vparamss, tpt, body1).duplicate
        }
        Template(parents1, self, defs ++ defs1)
    }
  }

  type Lifter = macro impl
}
