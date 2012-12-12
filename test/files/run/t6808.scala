import scala.reflect.runtime.universe._

class ann1(x: Int) extends scala.annotation.StaticAnnotation
class C1 @ann1(2) ()

class ann2(x: Int)(y: Int) extends scala.annotation.StaticAnnotation
class C2 @ann2(2)(3) ()

object Test extends App {
  println(typeOf[C1].typeSymbol.annotations)
  println(typeOf[C2].typeSymbol.annotations)
}