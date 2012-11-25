trait T

class C extends T
class D extends T()

object Test extends App {
  new T {}
  new T() {}

  new C
  new C()
}