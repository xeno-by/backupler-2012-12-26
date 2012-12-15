object Db extends Macros.H2Db("coffees")

object Test extends App {
  val all = Db.Coffees.all
  val brazilian = Db.Coffees.insert("Brazilian", 99, 0)
  Db.Coffees.update(brazilian.copy(price = 10))
  println(Db.Coffees.all)
}