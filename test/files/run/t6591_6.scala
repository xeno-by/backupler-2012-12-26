import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

class O { class I }

class A extends O {
  val x = new O
  val code = reify {
    val v: x.I = ???
    v
  }
  println(showRaw(code))
}

object Test extends App {
  val v = (new A).code.eval
}
