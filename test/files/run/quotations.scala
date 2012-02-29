import scala.reflect.mirror._  
import scala.reflect.internal._
import scala.tools.nsc
  

object Test extends App {       
  def simpleExpr() {
    val x = Literal(Constant(1))
	val y = Literal(Constant(2))
	println(c"$x + $y")
  }   
     
  def membersBody() {
     val mods = Flags.modifiersOfFlags(Flags.PARAM).toSet
  	 val app = Apply(Select(Ident(newTermName("x")), newTermName("$bang$eq")), List(Literal(Constant(null))))
  	 val notNull = 
       DefDef(Modifiers(mods), newTermName("notNull"), List(),
      	 List(List(ValDef(Modifiers(), newTermName("x"), Ident(newTypeName("String")), EmptyTree))),
         TypeTree(),
	     app)    
	                    
	 val cond = c"x.length <= 15" 
	 val otherwise = c"""println("fail")"""                  
	 val checkLength = 
	   c"""def checkLength(x: String) = 
			 if($cond) println("ok")
	         else
	           $otherwise"""
	 
	 val res = c"""class A { 
					 $notNull 
					 val x = 42 
					 $checkLength 
				   }""" 
	 println(res) 
  }
  
  def types() {
    val str = Literal(Constant("str"))
    val x = newTypeName("A") 
    val U = newTypeName("T1")   
    val V = newTypeName("T2")   
	val W = newTypeName("T3")                     
    println(c"""trait $x[T <: $U, -$V] extends $W { 
				  println($str + 1) 
				}""")
  }
   
  def block() {
    val rfl = newTermName("reflect")  
	val p = newTermName("z")
	val t = newTypeName("String")
    println(c"""import scala.$rfl._
				object Test { 
				  def y($p: $t) = println("Test " + $p)
				}""")
  }
  
  simpleExpr()                 
  membersBody()
  types()
  block()
  
  def matchSimpleExpr() {
    c"11 - (2 * 4) / 3" match {
	  case c"$x - $y / 3" => println(s"$x; $y")
	  case _ => println("fail")
	}
  }     

  def matchMembers() {
    c"""class A {
		  private val x = "x"
		  def y(z: Int) = z + x
		}""" match {
	  case c"""class A { 
	             $v
			     def y($p: $t) = $body
			   }""" => println(s"v = $v; p = $p; t = $t; body = $body")
	  case _ => 
	}
  }
  
  def matchBlock() {
    c"""import scala.reflect._
		class A { def id(z: Int) = z }
		""" match {
	  case c"""import scala.$rfl._
			   $clz""" => println(s"rfl = $rfl; clz = $clz")
	  case _ =>   
	}
  }
  
  matchSimpleExpr() 
  matchMembers()
  matchBlock()
}                                                