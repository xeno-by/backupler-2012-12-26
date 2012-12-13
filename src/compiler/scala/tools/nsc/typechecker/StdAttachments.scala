package scala.tools.nsc
package typechecker

trait StdAttachments {
  self: Analyzer =>

  import global._

  /** Carries information necessary to expand the host tree.
   *  At times we need to store this info, because macro expansion can be delayed until its targs are inferred.
   *  After a macro application has been successfully expanded, this attachment is destroyed.
   */
  type UnaffiliatedMacroContext = scala.reflect.macros.runtime.Context
  type MacroContext = UnaffiliatedMacroContext { val universe: self.global.type }
  case class MacroRuntimeAttachment(delayed: Boolean, typerContext: Context, macroContext: Option[MacroContext])

  /** Scratchpad for the macro expander, which is used to store everything except the details about the runtime.
   */
  case class MacroExpanderAttachment(original: Tree, role: MacroRole, enclosingTemplate: Tree)

  /** Loads underlying MacroExpanderAttachment from a macro expandee or returns a default value for that attachment.
   */
 def macroExpanderAttachment(tree: Tree): MacroExpanderAttachment =
    tree.attachments.get[MacroExpanderAttachment] getOrElse {
      tree match {
        case Apply(fn, _) if tree.isInstanceOf[ApplyToImplicitArgs] => macroExpanderAttachment(fn)
        case _ => MacroExpanderAttachment(EmptyTree, TERM_ROLE, EmptyTree)
      }
    }

  /** After being synthesized by the parser, primary constructors aren't fully baked yet.
   *  A call to super in such constructors is just a fill-me-in-later dummy resolved later
   *  by `parentTypes`. This attachment coordinates `parentTypes` and `typedTemplate` and
   *  allows them to complete the synthesis.
   */
  case class SuperArgsAttachment(argss: List[List[Tree]])

  /** Convenience method for `SuperArgsAttachment`.
   *  Compared with `MacroRuntimeAttachment` this attachment has different a usage pattern,
   *  so it really benefits from a dedicated extractor.
   */
  def superArgs(tree: Tree): Option[List[List[Tree]]] =
    tree.attachments.get[SuperArgsAttachment] collect { case SuperArgsAttachment(argss) => argss }

  /** Determines whether the given tree has an associated SuperArgsAttachment.
   */
  def hasSuperArgs(tree: Tree): Boolean = superArgs(tree).nonEmpty
}