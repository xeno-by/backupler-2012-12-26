import scala.tools.partest._
import java.io._

object Test extends DirectTest {
  def code = ???

  def macros_1 = """
    package test

    import scala.reflect.macros.Context
    import language.experimental.macros

    object Macros {
      def impl(c: Context) = {
        import c.universe._
        val Block(List(cdef: ClassDef), _) = reify{ class C }.tree
        if (!c.existsAmongTrees(TypeName("test.C"))) c.introduceTopLevel("test", cdef)
        Select(Ident(TermName("test")), TypeName("C"))
      }

      type Foo = macro impl
    }
  """
  def compileMacros() = {
    val classpath = List(sys.props("partest.lib"), sys.props("partest.reflect")) mkString sys.props("path.separator")
    compileString(newCompiler("-language:experimental.macros", "-cp", classpath, "-d", testOutput.path))(macros_1)
  }

  def test_2 = """
    package test
    object C extends Macros.Foo
  """
  def compileTest() = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(test_2)
  }

  def show(): Unit = {
    // redirect err to string, for logging
    val prevErr = System.err
    val baos = new ByteArrayOutputStream()
    System.setErr(new PrintStream(baos))
    log("Compiling Macros_1...")
    if (compileMacros()) {
      log("Compiling Test_2...")
      if (compileTest()) log("Success!") else log("Failed...")
    }
    println("""macroSynthetic-.*?\.scala""".r.replaceAllIn(baos.toString, "<synthetic file name>"))
    System.setErr(prevErr)
  }
}