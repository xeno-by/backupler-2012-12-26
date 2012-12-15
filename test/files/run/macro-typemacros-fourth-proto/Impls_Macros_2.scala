import scala.reflect.macros.Context
import language.experimental.macros
import java.sql._

object Macros {
  def impl(c: Context)(url: c.Expr[String]) = {
    import c.universe._
    import Flag._

    def connectionString(): String = {
      val Expr(Literal(Constant(sUrl: String))) = url
      val currentFile = c.enclosingUnit.source.file.path
      val pristineDb = new java.io.File(currentFile).getParent + "/" + sUrl + ".h2.db"
      val workingDb = new java.io.File(currentFile).getParent + "-run.obj/" + sUrl + ".h2.db"
      File.copy(pristineDb, workingDb)
      "jdbc:h2:" + workingDb.dropRight(".h2.db".length)
    }

    def generateCodeForTables(): List[Tree] = {
      Class.forName("org.h2.Driver")
      val conn = DriverManager.getConnection(connectionString(), "sa", "")
      try {
        val tables = Jdbc.list(conn, "show tables").map(_("table_name").asInstanceOf[String].toLowerCase)
        tables.flatMap(tbl => {
          // load table schema
          val columns = Jdbc.list(conn, "show columns from " + tbl).map(row => row("column_name").asInstanceOf[String].toLowerCase -> row("type").asInstanceOf[String])
          val schema = columns map { case (name, tpe) => name -> (tpe match {
            case s if s.startsWith("INTEGER") => Ident(newTypeName("Int"))
            case s if s.startsWith("VARCHAR") => Ident(newTypeName("String"))
            case s if s.startsWith("DOUBLE") => Ident(newTypeName("Double"))
          }) }
          val schemaWithoutId = schema filter { case (name, _) => name != "id" }

          // generate the dto case class
          val CASEACCESSOR = scala.reflect.internal.Flags.CASEACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
          val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
          val fields: List[Tree] = schema map { case (name, tpt) => ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), name, tpt, EmptyTree) }
          val ctorParams: List[ValDef] = schema map { case (name, tpt) => ValDef(Modifiers(PARAM | PARAMACCESSOR), name, tpt, EmptyTree) }
          val ctor: Tree = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
          val caseClass = ClassDef(Modifiers(CASE), newTypeName(tbl.capitalize.dropRight(1)), Nil, Template(
            List(Select(Ident(newTermName("scala")), newTypeName("Product")), Select(Ident(newTermName("scala")), newTypeName("Serializable"))),
            emptyValDef,
            fields :+ ctor))

          // generate the table object
          val tableSuper = Apply(AppliedTypeTree(Ident(newTypeName("Table")), List(Ident(caseClass.name))), List(Ident(newTermName("conn")), Literal(Constant(tbl))))
          val columnNamesWithoutId = schemaWithoutId map { case (name, _) => Literal(Constant(name)) }
          val resultSet = Select(Select(Ident(newTermName("java")), newTermName("sql")), newTypeName("ResultSet"))
          val createEntity = Apply(Ident(caseClass.name.toTermName), schema map {
            case (name, tpe) => Apply(Select(Ident(newTermName("rs")), "get" + tpe.name.toString), List(Literal(Constant(name))))
          })
          val insertParams = schemaWithoutId map { case (name, tpt) => ValDef(Modifiers(PARAM), name, tpt, EmptyTree) }
          val insertArgs = schemaWithoutId map { case (name, _) => Ident(newTermName(name)) }
          val tableModule = ModuleDef(NoMods, newTermName(tbl.capitalize), Template(List(tableSuper), emptyValDef, List(
            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
            DefDef(Modifiers(), newTermName("columnNamesWithoutId"), List(), List(), TypeTree(), Block(List(), Apply(Ident(newTermName("List")), columnNamesWithoutId))),
            DefDef(Modifiers(), newTermName("createEntity"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("rs"), resultSet, EmptyTree))), TypeTree(), Block(List(), createEntity)),
            DefDef(Modifiers(), newTermName("entityId"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("entity"), Ident(caseClass.name), EmptyTree))), TypeTree(), Block(List(), Select(Ident(newTermName("entity")), newTermName("id")))),
            DefDef(Modifiers(), newTermName("insert"), List(), List(insertParams), TypeTree(), Block(List(), Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), newTermName("insertImpl")), insertArgs)))
          )))

          // that's it
          List(caseClass, tableModule)
        })
      } finally {
        conn.close()
      }
    }

    val Expr(Block(List(ClassDef(_, _, _, Template(parents, self, body))), _)) = reify {
      class CONTAINER {
        Class.forName("org.h2.Driver")
        val conn = DriverManager.getConnection(c.literal(connectionString()).splice, "sa", "")
        // generated code will be spliced here
      }
    }

    val name = newTypeName(c.enclosingClass.asInstanceOf[NameTree].name + "_Generated")
    c.introduceTopLevel(ClassDef(NoMods, name, Nil, Template(parents, self, body ++ generateCodeForTables())))
    Ident(name)
  }

  type H2Db(url: String) = macro impl
}