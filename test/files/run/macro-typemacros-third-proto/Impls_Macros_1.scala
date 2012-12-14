import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    c.enclosingTemplate match {
      case Template(_, _, ctor :: defs) =>
        val defs1 = defs collect {
          case ddef @ DefDef(mods, name, tparams, vparamss, tpt, body) =>
            val future = Select(Select(Select(Ident(TermName("scala")), TermName("concurrent")), TermName("package")), TermName("future"))
            val Future = Select(Select(Ident(TermName("scala")), TermName("concurrent")), TypeName("Future"))
            val tpt1 = if (tpt.isEmpty) tpt else AppliedTypeTree(Future, List(tpt))
            val body1 = Apply(future, List(body))
            val name1 = TermName("async" + name.toString.capitalize)
            DefDef(mods, name1, tparams, vparamss, tpt1, body1).duplicate
        }
        Template(Nil, emptyValDef, ctor +: defs ::: defs1)
    }
  }

  type Lifter = macro impl
}
