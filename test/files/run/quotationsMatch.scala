import scala.reflect.mirror._  
import scala.reflect.internal._
import scala.tools.nsc
  

object Test extends App {
  def testIdent() {
    c"x + 1" match {
      case c"$y + 1" => println(y)
      case _ => 
    }
  }

  def testLiteral() {
    c"11 - (2 * 4) / 3" match {
      case c"$x - $y / 3" => println(s"$x; $y")
      case _ => 
    }
  } 

  def testApply() {
    c"f(x)" match {
      case c"$func(x)" => println(func)
      case _ =>
    }          
    
    c"f(x, 1)" match {
      case c"f($param, 1)" => println(param)
      case _ =>
   }
  }          

  def testSelect() {
    c"x.Y" match {
      case c"$item.Y" => println(item)  
      case _ =>
    }          
    c"x.Y" match {
      case c"x.$member" => println(member)
      case _ =>
    }
  }

  def testClassDef() {
    c"class C {}" match {
      case c"class $cName {}" => println(cName)
      case _ =>
    }          
    c"class C[T1, T2] {}" match {
      case c"class C[T1, $tParam] {}" => println(tParam)
      case _ =>
    }                    

    c"""class C { 
          val x = 42 
          def y = "y"}""" match {
      case c"""class C {
 			     val x = 42
                 $member }""" => println(member)
      case _ =>
    }   

    c"class C(x: Int) { def this() = this(0)}" match {
      case c"class C(x:Int) { $ctor }" => println(ctor)
      case _ => 
    }       
  }
  
  def testModuleDef() {
    c"object M {}" match {
      case c"object $mName {}" => println(mName)
      case _ =>
    }          
    
    c"""object M { def x() = "111" }""" match {
      case c"object M { $member }" => println(member)
      case _ =>
    }
  }            
  
  def testValDef() {
    c"val v = 1" match {
      case c"val $valName = 1" => println(valName)
      case _ =>
    }          
    
    c"val v = 2*y - 4" match {
      case c"val v = $body" => println(body)
      case _ =>
    }
    
    c"val v: Int = 1" match {
      case c"val v: $t = 1" => println(t)
      case _ => 
    }
  }

  def testDefDef() {
    c"def x() = {}" match {
      case c"def $defName() {}" => println(defName)
      case _ =>
    }          

    c"def x[T <: ScalaObject]() = {}" match {
      case c"def x[$tParam <: ScalaObject]() = {}" => println(tParam)
      case _ =>
    }          

    c"""def x() = println("in def")""" match {
      case c"def x() = $body" => println(body)
      case _ =>
    }
  }  

  def testTypeDef() {
    c"class C[T] {}" match {
      case c"class C[$tName] {}" => println(tName)
      case _ =>
    }          

    c"class C[List[Int]] {}" match {
      case c"class C[List[$tParam]] {}" => println(tParam)
      case _ => 
    }           
    
    c"class C[T >: Nothing <: ScalaObject] {}" match {
      case c"class C[T >: $left <: $right] {}" => println(s"$left; $right")
      case _ =>
    }
  } 

  def testImport() {
    c"import scala.reflect._" match {
      case c"import scala.$name._" => println(name)
      case _ =>
    }
  }        
  
  def testTemplate() {
    c"class C extends T2 {}" match {
      case c"class C extends $parent {}" => println(parent)
      case _ =>
    }          
   
    c"trait T { s => }" match {
      case c"trait T { $sName => }" => println(sName)
      case _ => 
    }
  }

  def testBlock() {
    c"""val x = "x"
        val y = "y" """ match {
      case c"""$statement
               val y = "y" """ => println(statement) 
      case _ =>  
    }
  } 

  def testCaseDef() {
    c"z match { case (x, y) => }" match {
      case c"z match { case $pat => }" => println(pat)
      case _ =>
    }          
  
    c"z match { case (v1, v2) if v1 == v2 => }" match {
      case c"z match { case (v1, v2) if $guard => }" => println(guard)
      case _ =>
    }
  
    c"""z match { case (x, _) => println("match succedeed") }""" match {
      case c"z match { case (x, _) => $body }" => println(body)
      case _ =>
    }
  }
  
  def testAlternative() {
    c"x match { case 1 | 2 => } " match {
      case c"x match { case 1 | $alternative => }" => println(alternative)
      case _ =>
    }
  }    

  def testStar() {
    c"x match { case List(1*) => }" match {
      case c"x match { case List($elem*) => }" => println(elem)
      case _ => 
    }
  }
  
  def testFunction() {
    c"(x: Int) => println(x)" match {
      case c"($pName: $pType) => println(x)" => println(s"$pName: $pType")
      case _ =>
    }          

    c"(x: Int) => println(x)" match {
      case c"(x: Int) => $body" => println(body)
      case _ =>
    }
  } 

  def testAssign() {
    c"x = null" match {
      case c"$left = null" => println(left)
      case _ =>
    }          

    c"x = null" match {
      case c"x = $right" => println(right)
      case _ =>
    }
  }  

  def testIf() {  
    val code = c"if (x == 0) 0 else y / x" 
    code match {
      case c"if($cond) 0 else y / x" => println(cond)
      case _ =>
    }          

    code match {
      case c"if(x == 0) $thenp else y / x" => println(thenp)
      case _ =>
    }          
 
    code match {
      case c"if(x == 0) 0 else $elsep" => println(elsep)
      case _ =>
    }
  }
  
  def testMatch() {
    c"x match { case 1 => }" match {
      case c"$selector match { case 1 => }" => println(selector)
      case _ =>
    }   
  } 
  
  def testReturn() {
    c"return 1" match {
      case c"return $expr" => println(expr)
      case _ =>
    }          
  }            

  def testTry() {
    val code = c"""try { println("block") } 
                   catch { case e => println(e) } 
                   finally { println("finally")} """ 
    code match {
      case c"""try {$block} catch { case e => println(e)} finally { println("finally")}""" =>
        println(block)
      case _ =>
    }          

    code match {
      case c"""try { println("block")} catch { case e => println(e)} finally { $finalizer}""" =>  
        println(finalizer)
      case _ =>
    }          
  }            
  
  def testThrow() {
    c"throw new Exception()" match {
      case c"throw $expr" => println(expr)
      case _ =>
    }          
  }            

  def testNew() {
    c"""new String("s")""" match {
      case c"""new $tName("s")""" => println(tName)
      case _ =>
    }
  } 
  
  def testTyped() {
    val code = c"x: String" 
    code match {
      case c"x: $tName" => println(tName)
      case _ =>
    }          

    code match {
      case c"$v: String" => println(v)
      case _ =>
    }
  } 

  def testAnnotated() {
    val code = c"f(x: @unchecked)" 
    code match {
      case c"f(x: @$annot)" => println(annot)
      case _ =>
    }          
  
    code match {
      case c"f($arg: @unchecked)" => println(arg)
      case _ =>
    }
  } 
   
  def testAppliedType() {
    val code = c"x: List[Int]" 
    code match {
      case c"x: $tpt[Int]" => println(tpt)
      case _ =>
    }          

    code match {
      case c"x: List[$arg]" => println(arg)
      case _ =>
    }
  } 
  
  def testTypeBounds() {
    c"trait X[T >: Tlo]" match {
      case c"trait X[T >: $lo]" => println(lo)
      case _ =>
    }          

    c"trait X[T <: Thi]" match {
      case c"trait X[T <: $hi]" => println(hi)
      case _ =>
    } 
  }
  
  def testSelectFromType() {
    val code = c"x: TQual#TName" 
    code match {
      case c"x: $qual#TName" => println(qual)
      case _ =>
    }          

    code match {
      case c"x: TQual#$name" => println(name)
      case _ =>
    }
  } 

  def testCompoundType() {
    c"def f[T <: { val x = 1 }]() {}" match {
      case c"def f[T <: $cmpTree]() {}" => println(cmpTree)
      case _ => 
    }
  }   

  def testExistentialType() {
    c"x: T1[_]" match {
      case c"x: $tName[_]" => println(tName)
      case _ =>
    }          

    c"x: T1[_ <: T2]" match {
      case c"x: T1[_ <: $tBound]" => println(tBound)
      case _ =>
    }
  }        

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


  def failingCases() {  
    ////the names can't start with an uppercase letter
    //c"val v: Int = 1" match {
    //  case c"val v: $T = 1" => println(T)
    //}
    //
    //// compilation fails with "pName is already defined as value pName"
    //c"def id(y: Int) = y" match {
    //  case c"def id($pName: $pType) = $pName" => println(s"$pName: $pType")
    //}
	//
	
    //// Modifiers
    //c"abstract class C {}" match {
    //  case c"$mod class C {}" => println(mod)
    //}
    //
    //// PackageDef - can't be parsed
    //c"package p" match {
    //  case c"package $pName" => println(pName)
    //}
    //
	//// DefDef
	//c"def f(x: Int) {}" match {
	//  case c"def f($vParam) {}" => println(vParam)
	//}
	//
	//// Bind
	//c"x match { case (_, y @ List(_)) => }" match {
	//  case c"x match { case (_, $elem @ List(_)) => }" => println(elem)
	//}
    //
    //// CaseDef
    //c"x match { case 1 => }" match {
    //  case c"x match { $caseDef }" => println(caseDef)
    //}
    //
   	//// CompoundType
	//c"def f[T <: { val x = 1 }]() {}" match {
	//  case c"def f[T <: { $templateM }]() {}" => println(templateM)
	//}
  }      
}                                                