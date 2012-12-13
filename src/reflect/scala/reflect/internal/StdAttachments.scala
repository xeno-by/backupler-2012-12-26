package scala.reflect
package internal

trait StdAttachments {
  self: SymbolTable =>

  /**
   * Common code between reflect-internal Symbol and Tree related to Attachments.
   */
  trait Attachable {
    protected var rawatt: scala.reflect.macros.Attachments { type Pos = Position } = NoPosition
    def attachments = rawatt
    def setAttachments(attachments: scala.reflect.macros.Attachments { type Pos = Position }): this.type = { rawatt = attachments; this }
    def updateAttachment[T: ClassTag](attachment: T): this.type = { rawatt = rawatt.update(attachment); this }
    def removeAttachment[T: ClassTag]: this.type = { rawatt = rawatt.remove[T]; this }

    // cannot be final due to SynchronizedSymbols
    def pos: Position = rawatt.pos
    def pos_=(pos: Position): Unit = rawatt = (rawatt withPos pos)
    def setPos(newpos: Position): this.type = { pos = newpos; this }
  }

  /** When present, indicates that the host `Ident` has been created from a backquoted identifier.
   */
  case object BackquotedIdentifierAttachment

  /** Stores the trees that give rise to a refined type to be used in reification.
   *  Unfortunately typed `CompoundTypeTree` is lacking essential info, and the reifier cannot use `CompoundTypeTree.tpe`.
   *  Therefore we need this hack (see `Reshape.toPreTyperTypeTree` for a detailed explanation).
   */
  case class CompoundTypeTreeOriginalAttachment(parents: List[Tree], stats: List[Tree])

  /** Is added by the macro engine to the results of macro expansions.
   *  Stores the original expandee as it entered the `macroExpand` function.
   */
  case class MacroExpansionAttachment(original: Tree)

  /** When present, suppresses macro expansion for the host.
   *  This is occasionally necessary, e.g. to prohibit eta-expansion of macros.
   *
   *  Does not affect expandability of child nodes, there's context.withMacrosDisabled for that
   *  (but think thrice before using that API - see the discussion at https://github.com/scala/scala/pull/1639).
   */
  case object SuppressMacroExpansionAttachment

  /** Suppresses macro expansion of the tree by putting SuppressMacroExpansionAttachment on it.
   */
  def suppressMacroExpansion(tree: Tree) = tree.updateAttachment(SuppressMacroExpansionAttachment)

  /** Unsuppresses macro expansion of the tree by removing SuppressMacroExpansionAttachment from it and its children.
   */
  def unsuppressMacroExpansion(tree: Tree): Tree = {
    tree.removeAttachment[SuppressMacroExpansionAttachment.type]
    tree match {
      case Apply(fn, _) if tree.isInstanceOf[ApplyToImplicitArgs] => unsuppressMacroExpansion(fn)
      case _ => // do nothing
    }
    tree
  }

  /** Determines whether a tree should not be expanded, because someone has put SuppressMacroExpansionAttachment on it or one of its children.
   */
  def isSuppressMacroExpansion(tree: Tree): Boolean =
    if (tree.attachments.get[SuppressMacroExpansionAttachment.type].isDefined) true
    else tree match {
      case Apply(fn, _) if tree.isInstanceOf[ApplyToImplicitArgs] => isSuppressMacroExpansion(fn)
      case _ => false
    }
}
