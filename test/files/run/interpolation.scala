object Test extends App {

  def test1(n: Int) = {
    println(s"Bob is $n years old")
    println(f"Bob is $n%2d years old")
    println(s"Bob will be ${n+1} years old")
    println(f"Bob will be ${n+1}%2d years old")
    println(s"$n+1 = ${n+1}")
    println(f"$n%d+1 = ${n+1}%d")
  }

  def test2(f: Float) = {
    println(s"Best price: $f")
    println(f"Best price: $f%.2f")
    println(s"$f% discount included")
    println(f"$f%3.2f% discount included")
  }

  def test3(z: Char, lzy: Boolean, str: String) = {
    val lzyIndicator = if (lzy) "?" else ""
    r"[a-$z]+$lzyIndicator" findFirstIn str match {
      case Some(s) => println(s"matches '$s'")
      case _ => println("no match found")
    }
  }
  
  test1(1)
  test1(12)
  test1(123)

  test2(10.0f)
  test2(13.345f)

  test3('z', true, "all lowercase letters a-z (lazy)")
  test3('z', false, "all lowercase letters a-z (greedy)")
  test3('t', false, "characters a-r (greedy)")
  test3('s', false, "z")
}
