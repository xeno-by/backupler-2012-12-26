package scala.tools.nsc

object QuotationContext {
  val quotationPrefix = "$$QUOTATION$$"
  
  implicit def StringContextToQuotation(ctx: StringContext) = new Global(new Settings) {
    import StringContext._
    import scala.reflect._

    override val currentRunId = 1
	phase = new syntaxAnalyzer.ParserPhase(NoPhase)
    
    def c(args: Any*): mirror.Tree = {
      if (ctx.parts.length != args.length + 1)
    	  throw new IllegalArgumentException("wrong number of arguments for quotation")
      
      val (code, quoteMap) = genCodeParams(ctx.parts.toList, args.toList, 0, "", Map())
      
      val location = this.getClass.getProtectionDomain.getCodeSource.getLocation
	  this.settings.classpath.value = location.getPath
	  
	  try {
	    val file = new util.BatchSourceFile("<quotation>", code)
	    val unit = new CompilationUnit(file)
	    val prsr = new syntaxAnalyzer.UnitParser(unit, List())
	    
	    val tree = prsr.templateStatSeq(false)._2.head
	    
	    val importer = new QuotationImporter(quoteMap)
	    return importer.importTree(tree).asInstanceOf[mirror.Tree]
	  } 
	  catch { case e => println (e.getMessage) }

	  mirror.EmptyTree
    }
    
    private def genCodeParams(parts: List[String], args: List[Any],  
        i: Int, code: String, quoteMap: Map[String, Any]): (String, Map[String, Any]) = 
          
      (parts, args) match {
      	case (p::Nil, _) => (code + p, quoteMap)
      	case (p::last::Nil, t::_) => 
      	  val quoteId = quotationPrefix + i
      	  (code + p + quoteId + last, quoteMap + (quoteId -> t))
      	case (p::ps, t::trees) =>
      	  val quoteId = quotationPrefix + i
      	  genCodeParams(ps, trees, i+1, code + p + quoteId, quoteMap + (quoteId -> t)) 
      	case _ => (code, quoteMap)
      }
       
    val mirror = scala.reflect.mirror.asInstanceOf[scala.reflect.runtime.Mirror]
    
    class QuotationImporter(quotations: Map[String, Any]) extends mirror.Importer {
      val from: syntaxAnalyzer.global.type = syntaxAnalyzer.global
      
      override def importTypeName(name: from.TypeName): mirror.TypeName ={
        quotations.get(name.toString) match {
          case Some(t: mirror.TypeName) => t
          case _ => super.importTypeName(name)
        }}
        
      override def importTermName(name: from.TermName): mirror.TermName = {
        quotations.get(name.toString) match {
          case Some(t: mirror.TermName) => t
          case _ => super.importTermName(name)
        }}
      
      override def importName(name: from.Name): mirror.Name = 
	    quotations.get(name.toString) match {
	      case Some(t: mirror.Name) => t
	      case _ => super.importName(name)
	    }

	  override def importTree(tree: from.Tree): mirror.Tree = tree match {
	        case from.Ident(name) => 
	          quotations.get(name.toString) match {
	        	case Some(t: mirror.Name) => mirror.Ident(t)
	        	case Some(t: mirror.Tree) => t
	        	case Some(t) => 
	        	  throw new IllegalArgumentException("quotation argument should be of type Tree or Name")
	        	case _ => 
	        	  super.importTree(tree).asInstanceOf[mirror.Ident]
	        }
	        case _ => super.importTree(tree)
	      }
    }
  }
}
