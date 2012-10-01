package scala.reflect
package api

/** A slice of the Scala reflection cake that defines importers and operations on them.
 *
 *  As described in [[scala.reflect.api.package]], reflection artifacts are contained in universes.
 *  Typically all processing happens within a single universe (e.g. a compile-time macro universe
 *  or a runtime reflection universe), but sometimes there is a need to migrate artifacts from
 *  one universe to another (e.g. runtime compilation works by importing runtime reflection trees
 *  into a runtime compiler universe, compiling the importees and exporting the result back).
 *
 *  Reflection artifacts are firmly grounded in their universes, it is impossible
 *  to just move them around. Instead importers locate or recreate corresponding artifacts
 *  in the target universe. For example, to import `foo.bar.Baz` from the source universe to the target universe,
 *  an importer will first check whether the entire owner chain exists in the target universe.
 *  If it does, then nothing else will be done. Otherwise, the importer will recreate the entire owner chain
 *  and will import the corresponding type signaturers into the target universe.
 *
 *  Since importers match symbol tables of the source and the target universes using plain string names,
 *  it is programmer's responsibility to make sure that imports don't distort semantics (e.g. that
 *  `foo.bar.Baz` in the source universe means the same that `foo.bar.Baz` does in the target universe).
 *
 *  Known issue: importers didn't undergo as much testing as most of the reflection API did,
 *  so they might be flaky from time to time. Please report issues if you encounter them.
 *
 *  Known issue: importers are currently not mirror-aware, they always use `rootMirror`
 *  of the target universe to resolve symbols. This might cause troubles in cases when the target universe
 *  need a non-standard way of symbol resolution (e.g. a classloader that's different from the default one).
 *  We have created [[https://issues.scala-lang.org/browse/SI-6241 https://issues.scala-lang.org/browse/SI-6241]],
 *  an issue in the issue tracker, to track the implementation of this feature.
 *
 *  Here's how one might implement a macro that performs compile-time evaluation of its argument
 *  by using a runtime compiler to compile and evaluate a tree that belongs to a compile-time compiler:
 *
 *  {{{
 *  def staticEval[T](x: T) = macro staticEval[T]
 *
 *  def staticEval[T](c: scala.reflect.macros.Context)(x: c.Expr[T]) = {
 *    // creates a runtime reflection universe to host runtime compilation
 *    import scala.reflect.runtime.{universe => ru}
 *    val mirror = ru.runtimeMirror(c.libraryClassLoader)
 *    import scala.tools.reflect.ToolBox
 *    val toolBox = mirror.mkToolBox()
 *
 *    // runtime reflection universe and compile-time macro universe are different
 *    // therefore an importer is needed to bridge them
 *    // currently mkImporter requires a cast to correctly assign the path-dependent types
 *    val importer0 = ru.mkImporter(c.universe)
 *    val importer = importer0.asInstanceOf[ru.Importer { val from: c.universe.type }]
 *
 *    // the created importer is used to turn a compiler tree into a runtime compiler tree
 *    // both compilers use the same classpath, so semantics remains intact
 *    val imported = importer.importTree(tree)
 *
 *    // after the tree is imported, it can be evaluated as usual
 *    val tree = toolBox.resetAllAttrs(imported.duplicate)
 *    val valueOfX = toolBox.eval(imported).asInstanceOf[T]
 *    ...
 *  }
 *  }}}
 */
trait Importers { self: Universe =>

  /** Creates an importer that moves reflection artifacts between universes. */
  def mkImporter(from0: Universe): Importer { val from: from0.type }

  /** The API of importers.
   *  The main source of information about importers is the [[scala.reflect.api.Importers]] page.
   */
  trait Importer {
    /** The source universe of reflection artifacts that will be processed.
     *  The target universe is universe that created this importer with `mkImporter`.
     */
    val from: Universe

    /** An importer that works in reverse direction, namely:
     *  imports reflection artifacts from the current universe to the universe specified in `from`.
     */
    val reverse: from.Importer { val from: self.type }

    /** In the current universe, locates or creates a symbol that corresponds to the provided symbol in the source universe.
     *  If necessary imports the owner chain, companions, type signature, annotations and attachments.
     */
    def importSymbol(sym: from.Symbol): Symbol

    /** In the current universe, locates or creates a type that corresponds to the provided type in the source universe.
     *  If necessary imports the underlying symbols, annotations, scopes and trees.
     */
    def importType(tpe: from.Type): Type

    /** In the current universe, creates a tree that corresponds to the provided tree in the source universe.
     *  If necessary imports the underlying symbols, types and attachments.
     */
    def importTree(tree: from.Tree): Tree

    /** In the current universe, creates a position that corresponds to the provided position in the source universe.
     */
    def importPosition(pos: from.Position): Position
  }
}