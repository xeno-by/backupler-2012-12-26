package scala.tools.nsc

class QuotationContext(parts: String*) extends Global(new Settings) with scala.QuotationCtx {
  import StringContext._
  import scala.reflect._
  import matcher._
  
  override val currentRunId = 1
  phase = new syntaxAnalyzer.ParserPhase(NoPhase)

  def parseTree(code: String, quoteMap: Map[String, Any]): mirror.Tree = {    
    val location = this.getClass.getProtectionDomain.getCodeSource.getLocation
	settings.classpath.value = location.getPath
	  
	try {
	  val file = new util.BatchSourceFile("<quotation>", code)
	  val unit = new CompilationUnit(file)
	  val prsr = new syntaxAnalyzer.UnitParser(unit, List())
	  
	  val trees = prsr.templateStatSeq(false)._2
	  val tree = 
	    if(trees.length == 1) trees.head
	    else
	      syntaxAnalyzer.global.Block(trees: _*)
	  
	  val importer = new QuotationImporter(quoteMap)
	  return importer.importTree(tree).asInstanceOf[mirror.Tree]
	} 
	catch { case e => println (e.getMessage) }

	mirror.EmptyTree
  }

  def apply(args: Any*): mirror.Tree = {
    if (parts.length != args.length + 1)
      throw new IllegalArgumentException("wrong number of arguments for quotation")
      
    val (code, quoteMap) = genCodeParams(parts.toList, args.toList, 0, "", Map())
    parseTree(code, quoteMap)
  }   
      
  def unapplySeq(pattern: mirror.Tree): Option[Seq[Any]] = {
    val code = 
      ((parts.head, 0) /: parts.tail)((acc, s) => 
          (acc._1 + quotationPrefix + acc._2 + s, acc._2+1))._1
             
    matcher.matchTree(parseTree(code, Map()), pattern, List()) match {
      case Some(quotations) => 
        Some(quotations sortBy (_._1) map (_._2))
      case _ => None
    }
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
       
  val mrr = scala.reflect.mirror.asInstanceOf[scala.reflect.runtime.Mirror]
    
  class QuotationImporter(quotations: Map[String, Any]) extends mrr.Importer {
    val from: syntaxAnalyzer.global.type = syntaxAnalyzer.global
      
    override def importTypeName(name: from.TypeName): mrr.TypeName =
      if (name == null) null
      else
        quotations.get(name.toString) match {
          case Some(t: mrr.TypeName) => t
          case _ => super.importTypeName(name)
        }
        
    override def importTermName(name: from.TermName): mrr.TermName = 
      if (name == null) null
      else
        quotations.get(name.toString) match {
          case Some(t: mrr.TermName) => t
          case _ => super.importTermName(name)
        }
      
    override def importName(name: from.Name): mrr.Name = 
      if (name == null) null
      else
	    quotations.get(name.toString) match {
	      case Some(t: mrr.Name) => t
	      case _ => super.importName(name)
	    }
    
    //def importTreeList(trees: List[from.Tree]): List[mrr.Tree] = 
    //  trees flatMap {
    //    case (t @ from.Ident(name)) => 
    //      quotations.get(name.toString) match {
    //        case Some(ts: List[Tree]) => ts.asInstanceOf[List[mrr.Tree]]
    //        case _ => List(importTree(t))
    //      }
    //    case t => List(importTree(t))
    //  }
      
    override def importTree(tree: from.Tree): mrr.Tree = tree match {
	  case from.Ident(name) => 
	    quotations.get(name.toString) match {
	      case Some(t: mrr.Name) => mrr.Ident(t)
	      case Some(t: mrr.Tree) => t
	      case Some(t) => 
	        throw new IllegalArgumentException("quotation argument should be of type Tree or Name")
	      case _ => 
	        super.importTree(tree).asInstanceOf[mrr.Ident]
	     }
	  //case from.Template(parents, self, body) =>
	  //  new mrr.Template(importTreeList(parents), importValDef(self), importTreeList(body))
	  case _ => super.importTree(tree)
    }
  }
} 

object matcher {
  import scala.reflect.mirror._
  val quotationPrefix = "$$QUOTATION$$"
        
  def mapResults[T](res: List[(String, Any)], opts: Option[List[(String, Any)]]*) = {
    if(opts.contains(None)) None
    else 
      Some (opts.toList flatMap (_.get))
  }  
       
  def matchTree(t: Tree, pattern: Tree,
      res: List[(String, Any)] = List()): Option[List[(String, Any)]] = (t, pattern) match {

    case (Ident(name), t) if name.toString startsWith quotationPrefix => 
      Some((name.toString, t)::res)
    case (Ident(name), Ident(pName)) => 
      mapResults(res, matchName(name, pName))
    case (Apply(fun, args), Apply(pFun, pArgs)) =>
      mapResults(res, matchTree(fun, pFun), matchTreeList(args, pArgs))  
    case (Select(q, name), Select(pq, pName)) => 
      mapResults(res, matchName(name, pName), matchTree(q, pq))
    case (ClassDef(mods, name, tparams, impl), ClassDef(pMods, pName, pTparams, pImpl)) 
      if mods == pMods =>
        mapResults(res, matchName(name, pName), matchTreeList(tparams, pTparams), matchTree(impl, pImpl))
    case (PackageDef(id, stats), PackageDef(pId, pStats)) =>
      mapResults(res, matchTree(id, pId), matchTreeList(stats, pStats))
    case (ModuleDef(mods, name, impl), ModuleDef(pMods, pName, pImpl)) 
      if mods == pMods =>
        mapResults(res, matchName(name, pName), matchTree(impl, pImpl))
    case (ValDef(mods, name, tpt, rhs), ValDef(pMods, pName, pTpt, pRhs))
      if mods == pMods =>
        mapResults(res, matchName(name, pName), matchTree(tpt, pTpt), matchTree(rhs, pRhs))
    case (DefDef(mods, name, tparams, vparamss, tpt, rhs), DefDef(pMods, pName, pTparams, pVparamss, pTpt, pRhs))
      if mods == pMods =>
        mapResults(res,
            matchName(name, pName),
            matchTreeList(tparams, pTparams), 
            matchTreeList(vparamss flatMap {x => x}, pVparamss flatMap {x => x}),
            matchTree(tpt, pTpt),
            matchTree(rhs, pRhs))
    case (TypeDef(mods, name, tparams, rhs), TypeDef(pMods, pName, pTparams, pRhs)) 
      if mods == pMods =>
        mapResults(res, matchName(name, pName), matchTreeList(tparams, pTparams), matchTree(rhs, pRhs))
    case (LabelDef(name, params, rhs), LabelDef(pName, pParams, pRhs)) =>
      mapResults(res, matchName(name, pName), matchTreeList(params, pParams), matchTree(rhs, pRhs))
    case (Import(expr, selectors), Import(pExpr, pSelectors)) => 
      mapResults(res, matchImportSelectors(selectors, pSelectors), matchTree(expr, pExpr))
    case (Template(parents, self, body), Template(pParents, pSelf, pBody)) =>
      mapResults(res, 
          matchTreeList(parents, pParents), 
          matchTree(self, pSelf), 
          matchTreeList(body, pBody)) 
    case (Block(stats, expr), Block(pStats, pExpr)) =>
      mapResults(res, matchTreeList(stats, pStats), matchTree(expr, pExpr))
    case (CaseDef(pat, guard, body), CaseDef(pPat, pGuard, pBody)) =>
      mapResults(res, matchTree(pat, pPat), matchTree(guard, pGuard), matchTree(body, pBody))
    case (Alternative(trees), Alternative(pTrees)) =>
      mapResults(res, matchTreeList(trees, pTrees))
    case (Star(elem), Star(pElem)) =>
      mapResults(res, matchTree(elem, pElem))
    case (Bind(name, body), Bind(pName, pBody))=>
      mapResults(res, matchName(name, pName), matchTree(body, pBody))
    case (UnApply(fun, args), UnApply(pFun, pArgs)) =>
      mapResults(res, matchTree(fun, pFun), matchTreeList(args, pArgs))
    case (ArrayValue(elemtpt, elems), ArrayValue(pElemtpt, pElems)) =>
      mapResults(res, matchTree(elemtpt, pElemtpt), matchTreeList(elems, pElems))
    case (Function(vparams, body), Function(pVparams, pBody)) =>
      mapResults(res, matchTreeList(vparams, pVparams), matchTree(body, pBody))
    case (Assign(lhs, rhs), Assign(pLhs, pRhs)) =>
      mapResults(res, matchTree(lhs, pLhs), matchTree(rhs, pRhs))
    case (If(cond, thenp, elsep), If(pCond, pThenp, pElsep)) =>
      mapResults(res, 
          matchTree(cond, pCond), 
          matchTree(thenp, pThenp), 
          matchTree(elsep, pElsep))
    case (Match(selector, cases), Match(pSelector, pCases)) =>
      mapResults(res, matchTree(selector, pSelector), matchTreeList(cases, pCases))
    case (Return(expr), Return(pExpr)) =>
      mapResults(res, matchTree(expr, pExpr))
    case (Try(block, catches, finalizer), Try(pBlock, pCatches, pFinalizer)) =>
      mapResults(res, 
          matchTree(block, pBlock), 
          matchTreeList(catches, pCatches),
          matchTree(finalizer, pFinalizer))
    case (Throw(expr), Throw(pExpr)) =>
      mapResults(res, matchTree(expr, pExpr))
    case (New(tpt), New(pTpt)) =>
      mapResults(res, matchTree(tpt, pTpt))
    case (Typed(expr, tpt), Typed(pExpr, pTpt)) =>
      mapResults(res, matchTree(expr, pExpr), matchTree(tpt, pTpt))
    case (TypeApply(fun, args), TypeApply(pFun, pArgs)) =>
      mapResults(res, matchTree(fun, pFun), matchTreeList(args, pArgs))
    case (ApplyDynamic(qual, args), ApplyDynamic(pQual, pArgs)) =>
      mapResults(res, matchTree(qual, pQual), matchTreeList(args, pArgs))
    case (Super(qual, mix), Super(pQual, pMix)) => 
      mapResults(res, matchName(mix, pMix), matchTree(qual, pQual))
    case (This(qual), This(pQual)) =>
      mapResults(res, matchName(qual, pQual))
    case (Literal(Constant(c1)), Literal(Constant(c2))) if c1 == c2 =>
      Some(res)
    case (TypeTree(), TypeTree()) =>
      Some(res)
    case (Annotated(annot, arg), Annotated(pAnnot, pArg)) =>
      mapResults(res, matchTree(annot, pAnnot), matchTree(arg, pArg))
    case (SingletonTypeTree(ref), SingletonTypeTree(pRef)) =>
      mapResults(res, matchTree(ref, pRef))
    case (SelectFromTypeTree(qual, name), SelectFromTypeTree(pQual, pName)) =>
      mapResults(res, matchName(name, pName), matchTree(qual, pQual))
    case (CompoundTypeTree(templ), CompoundTypeTree(pTempl)) =>
      mapResults(res, matchTree(templ, pTempl))
    case (AppliedTypeTree(tpt, args), AppliedTypeTree(pTpt, pArgs)) =>
      mapResults(res, matchTree(tpt, pTpt), matchTreeList(args, pArgs))
    case (TypeBoundsTree(lo, hi), TypeBoundsTree(pLo, pHi)) =>
      mapResults(res, matchTree(lo, pLo), matchTree(hi, pHi))
    case (ExistentialTypeTree(tpt, whereClauses), ExistentialTypeTree(pTpt, pWhereClauses)) =>
      mapResults(res, matchTree(tpt, pTpt), matchTreeList(whereClauses, pWhereClauses))
    case (t, pt) if t.isEmpty && pt.isEmpty =>
      Some(res)
    case (null, null) =>
      Some(res)
    case _ => None
   }
   
   def matchTreeList(xs: List[Tree], pxs: List[Tree]): Option[List[(String, Any)]] =
     if(xs.length != pxs.length) 
       None
     else {
       val res = xs zip pxs map { case (x, px) => matchTree(x, px, List()) }
       if (res.contains(None)) None
       else 
         Some(res flatMap (_.get))
   }
  
   def matchName(name: Name, pName: Name) = 
     if (name == pName)
       Some(List())
     else if (name.toString startsWith quotationPrefix)
       Some(List((name.toString, pName)))
     else
       None
      
   def matchImportSelectors(selectors: List[ImportSelector], pSelectors: List[ImportSelector]) = { 
     var matchOk = true
     val res = selectors zip pSelectors map {
       case (ImportSelector(name, npos, rename, rpos),
             ImportSelector(pName, pNpos, pRename, pRpos)) if rpos == pRpos =>
         mapResults(List(), matchName(name, pName), matchName(rename, pRename))      
       case _ =>
         matchOk = false
         None
     }
     if (matchOk)
       Some(res flatMap (_.get))
     else
       None
   }
}
