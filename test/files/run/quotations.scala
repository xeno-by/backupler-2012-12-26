import scala.reflect.mirror._  
import scala.reflect.internal._
import scala.tools.nsc
  

object Test extends App { 
  def testIdent() {
    val x = Ident(newTermName("y"))
    println(c"$x^2") 
  }

  def testLiteral() {
    val x = Literal(Constant(1))
	val y = Literal(Constant(2))
	println(c"$x + $y")
  }            
    
  def testApply() {
	val func = newTermName("f")
	println(c"$func(x)")
	
	val param = newTermName("x")
	println(c"f($param, 1)")
  }
      
  def testSelect() {
    val y = newTermName("Y")
	val x = Ident(newTermName("x"))
    println(c"x.$y")               
	println(c"$x.Y")
  }    

  def testClassDef() {                            
    val CName = newTypeName("C")
	println(c"class $CName {}")
           
    val TParam = newTypeName("T2")
	println(c"class C[T1, $TParam] {}")    
	
	val member = c"val x = 42" 
	println(c"class C { $member }") 
	
	val ctor = c"def this() = this(0)"  
	println(c"class C(x: Int) { $ctor }")
  }                                 
  
  def testModuleDef() {
    val mName = newTypeName("M")
    println(c"object $mName {}")    
       
    val member = c"""def x() = "111" """
	println(c"object M { $member }")
  }
     
  def testValDef() {
    val valName = newTermName("v")
    println(c"val $valName = 1")

    val body = c"2*y - 4" 
	println(c"val v = $body") 
	
	val t = newTypeName("Int")
	println(c"val v: $t = 1")
  }  

  def testDefDef() {
    val defName = newTermName("x")
    println(c"def $defName() = {}")

    val tParam = newTypeName("T")
    println(c"def x[$tParam <: ScalaObject]() = {}")
	
	val pName = newTermName("y")       
	val pType = newTypeName("Int")
	println(c"def id($pName: $pType) = $pName")
	
	val body = c"""println("in def")"""
	println(c"def x() = $body")
  }
   
  def testTypeDef() {
    val tName = newTypeName("T")
	println(c"class C[$tName] {}")
	
	val tParam = newTypeName("Int")
	println(c"class C[List[$tParam]] {}")
	              
	val left = newTypeName("Nothing")
	val right = newTypeName("ScalaObject")
	println(c"class C[T >: $left <: $right ]{ }")
  }  

  def testImport() {
    val name = newTermName("reflect")
	println(c"import scala.$name._")
  }
  
  def testTemplate() {
	val parent = newTypeName("T2") 
	println(c"class C extends $parent { }")  
                 
    val sName = newTermName("s")
	println(c"trait T { $sName => }")
  } 

  def testBlock() {
    val statement = c"""val x = "x""""
    println(c""" $statement
				 val y = "y" """)
  }
  
  def testCaseDef() {
    val pat = c"(x, y)"  
  	println(c"z match { case $pat => }")
    
	val (v1, v2) = (newTermName("x"), newTermName("y"))
    val guard = c"$v1 == $v2" 
    println(c"z match { case ($v1, $v2) if $guard  => }")    

    val body = c"""println("match succedeed")""" 
    println(c"z match { case (`x`, _) => $body }")
  }
     
  def testAlternative() {  
    val alternative = Literal(Constant(2))
    println(c"""x match { 
					case 1 | $alternative => "ok" 
					case _ => "fail"}""")
  } 

  def testStar() {
	val elem = Literal(Constant(1))
	println(c"x match { case List($elem*) => }")
  }

  def testFunction() {   
	val pName = newTermName("x")
	val pType = newTypeName("Int")
  	println(c"($pName: $pType) => println($pName)")

    val body = c"println(x)" 
    println(c"(x: Int) => $body")  
  } 

  def testAssign() {
    val left = newTermName("x")
    println(c"$left = null")

    val right = Literal(Constant(null))
    println(c"x = $right")
  }
   
  def testIf() {
	val cond = c"x == 0" 
	println(c"if($cond) 0 else y / x")
	
	val thenp = Literal(Constant(0))
	println(c"if(x == 0) $thenp else y / x")
	
	val elsep = c"y / x" 
	println(c"if(x == 0) 0 else $elsep")
  }
  
  def testMatch() {
    val selector = newTermName("x")
	println(c"$selector match { case 1 => }")  
  }   

  def testReturn() {
    val expr = Literal(Constant(1))
	println(c"return $expr")
  }   

  def testTry() {
    val block = c"""println("block")""" 
    println(c"""try { $block } catch { case e => println(e) } finally { println("finally") }""")     
   
    val finalizer = c"""println("finally")""" 
    println(c"""try { println("block") } finally { $finalizer }""") 
  } 

  def testThrow() {
    val expr = c"new Exception()" 
    println(c"throw $expr")
  }
  
  def testNew() {
    val tName = newTypeName("String")
    println(c"""new $tName("s")""")  
  }
  
  def testTyped() {
    val tName = newTypeName("String")
	println(c"x: $tName")
	
	val v = newTermName("x")
	println(c"$v: String")
  }  

  def testAnnotated() {
    val annot = newTermName("unchecked")
    println(c"f(x: @$annot)")

    val arg = newTermName("x")
    println(c"f($arg: @unchecked)")
  }
   
  def testAppliedType() {
    val tpt = newTypeName("List")
    println(c"x: $tpt[Int]")
   
    val arg = newTypeName("Int")
    println(c"x: List[$arg]")
  } 
 
  def testTypeBounds() { 
    val lo = newTypeName("Tlo")
    println(c"trait X[T >: $lo] {}")
   
    val hi = newTypeName("Thi")
    println(c"trait X[T <: $hi] {}")
  }
   
  def testSelectFromType() {
    val qual = newTypeName("TQual")
    println(c"x: $qual#TName")

    val name = newTypeName("TName")
    println(c"x: TQual#$name")
  } 

  def testCompoundType() { 
	val cmpT = CompoundTypeTree(Template(List(), emptyValDef, List(c"val x = 1")))
    println(c"def f[T <: $cmpT]() {}")
  }  

  def testExistentialType() {
    val tName = newTypeName("T1")
    println(c"x: $tName[_]")

    val tBound = newTypeName("T2")
    println(c"x: T1[_ <: $tBound]")
  } 
  
  // can't be parsed
  //def failingCases() {
  //  // Modifiers
  //  val mod = Flags.modifiersOfFlags(Flags.ABSTRACT).toSet  
  //  println(c"$mod class C {}")
  //  
  //  // PackageDef
  //val packageName = newTermName("p")
  //println(c"package $packageName")
  //
  //  // DefDef
  //  val vParam = c"val y = 1" 
  //  println(c"def f($vParam) {}")
  //  
  //  // Bind - quotation prefix has both uppercase letters and $ 
  //  val elem = newTermName("y")
  //  println(c"x match { case (_, $elem @ List(_)) => }")
  //  
  //  // CaseDef
  //  val caseDef = CaseDef(Literal(Constant(1)), EmptyTree, EmptyTree)
  //  println(c"x match { $caseDef }")
  //  
  //  // CompoundType
  //  val templateM = c"val x = 1" 
  //  println(c"def f[T <: { $templateM }]() {}")
  //}

  testIdent()
  testLiteral()  
  testApply() 
  testSelect()  
  testClassDef()
  testModuleDef()
  testValDef() 
  testDefDef()
  testTypeDef()
  testImport()
  testTemplate()   
  testBlock()
  testCaseDef()
  testAlternative()
  testStar()
  testFunction()
  testAssign() 
  testIf() 
  testMatch() 
  testReturn()
  testTry()
  testThrow()
  testNew()
  testTyped() 
  testAnnotated()
  testAppliedType()
  testTypeBounds()
  testSelectFromType()
  testCompoundType()
  testExistentialType()
}                                                