package scala.reflect.macros
package runtime

trait Enclosures {
  self: Context =>

  import universe._
  import mirror._

  type MacroRole = analyzer.MacroRole
  def TERM_ROLE = analyzer.TERM_ROLE
  def TYPE_ROLE = analyzer.TYPE_ROLE
  def APPLIED_TYPE_ROLE = analyzer.APPLIED_TYPE_ROLE
  def PARENT_ROLE = analyzer.PARENT_ROLE
  def NEW_ROLE = analyzer.NEW_ROLE
  def ANNOTATION_ROLE = analyzer.ANNOTATION_ROLE

  private def site       = callsiteTyper.context
  private def enclTrees  = site.enclosingContextChain map (_.tree)
  private def enclPoses  = enclosingMacros map (_.macroApplication.pos) filterNot (_ eq NoPosition)

  // vals are eager to simplify debugging
  // after all we wouldn't save that much time by making them lazy
  val macroApplication: Tree                 = expandee
  val enclosingClass: Tree                   = enclTrees collectFirst { case x: ImplDef => x } getOrElse EmptyTree
  val enclosingTemplate: Tree                = analyzer.macroExpanderAttachment(expandee).enclosingTemplate orElse
                                               (enclTrees collectFirst { case x: Template => x } getOrElse EmptyTree)
  val enclosingImplicits: List[(Type, Tree)] = site.openImplicits
  val enclosingMacros: List[Context]         = this :: universe.analyzer.openMacros // include self
  val enclosingMethod: Tree                  = site.enclMethod.tree
  val enclosingPosition: Position            = if (enclPoses.isEmpty) NoPosition else enclPoses.head.pos
  val enclosingUnit: CompilationUnit         = universe.currentRun.currentUnit
  val enclosingRun: Run                      = universe.currentRun
}
