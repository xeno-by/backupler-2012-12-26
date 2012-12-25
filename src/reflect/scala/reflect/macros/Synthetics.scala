package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that
 *  exposes functions to introduce synthetic definitions.
 */
trait Synthetics {
  self: Context =>

  /** Checks whether a definition with a given fully-qualified name (term name for modules, type name for classes)
   *  exists at top level in the sources (user-level or synthetic) comprising the current compilation run.
   */
  def existsAmongTrees(name: Name): Boolean

  /** Checks whether a definition with a given fully-qualified name (term name for modules, type name for classes)
   *  exists at top level on the classpath, i.e. is defined as a non-inner class in a classfile.
   */
  def existsOnClassPath(name: Name): Boolean

  /** Adds a top-level definition or a list of top-level definitions to the compiler's symbol table.
   *  Allowed definitions include classes (represented by `ClassDef` trees), traits (represented
   *  by `ClassDef` trees having the `TRAIT` flag set in `mods`) and objects (represented by `ModuleDef` trees).
   *
   *  The definitions are put into the package with a fully-qualified name provided in `packageName`.
   *  For example, to generate a class available at `foo.bar.Test`, call this method as follows:
   *
   *    introduceTopLevel("foo.bar", ClassDef(<mods>, TypeName("Test"), <tparams>, <template>))
   *
   *  It is possible to add definitions to the empty package by using `nme.EMPTY_PACKAGE_NAME.toString`, but
   *  that's not recommended, since such definitions cannot be seen from outside the empty package.
   */
  def introduceTopLevel(packageName: String, definitions: universe.ImplDef*): Unit
}
