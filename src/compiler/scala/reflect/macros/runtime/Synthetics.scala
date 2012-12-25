/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.macros
package runtime

import java.util.UUID._
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.MissingRequirementError
import scala.reflect.io.VirtualFile

trait Synthetics {
  self: Context =>

  import global._

  private def existsAtTopLevel(name: Name)(pathFilter: String => Boolean): Boolean = {
    val sym = {
      // getClassIfDefined and getModuleIfDefined cannot be used here
      // because they don't work for stuff declared in the empty package
      // (as specified in SLS, code inside non-empty packages cannot see
      // declarations from the empty package, so compiler internals
      // default to ignoring contents of the empty package)
      // to the contrast, staticModule and staticClass are designed
      // to be a part of the reflection API and, therefore, they
      // correctly resolve all names
      try {
        if (name.isTermName) mirror.staticModule(name.toString)
        else mirror.staticClass(name.toString)
      } catch {
        case MissingRequirementError(_) => NoSymbol
      }
    }
    val file = sym.associatedFile
    val path = if (file != null) file.path else null
    pathFilter(if (path != null) path else "")
  }

  def existsAmongTrees(name: Name): Boolean = existsAtTopLevel(name)(path => path.endsWith(".scala"))

  def existsOnClassPath(name: Name): Boolean = existsAtTopLevel(name)(path => path.endsWith(".class"))

  def introduceTopLevel(packageName: String, definitions: ImplDef*): Unit = {
    def mkPid(fragments: List[String]): RefTree = fragments match {
      case hd :: Nil => Ident(newTermName(hd))
      case hd :+ tl => Select(mkPid(hd), newTermName(tl))
    }
    val pid = mkPid(packageName.split(java.util.regex.Pattern.quote(".")).toList)
    val code = PackageDef(pid, definitions.toList)
    // TODO: provide a way to specify a pretty name for debugging purposes
    val syntheticFileName = "macroSynthetic-" + randomUUID().toString.replace("-", "") + ".scala"
    // compatibility with SBT
    // on the one hand, we need to specify some jfile here, otherwise sbt crashes with an NPE (SI-6870)
    // on the other hand, we can't specify the obvious enclosingUnit, because then sbt somehow fails to run tests using type macros
    // okay, now let's specify a guaranteedly non-existent file in an existing directory (so that we don't run into permission problems)
    val relatedJfile = enclosingUnit.source.file.file
    val fakeJfile = if (relatedJfile != null) new java.io.File(relatedJfile.getParent, syntheticFileName) else null
    val virtualFile = new VirtualFile(syntheticFileName) { override def file = fakeJfile }
    val sourceFile = new BatchSourceFile(virtualFile, code.toString)
    val unit = new CompilationUnit(sourceFile)
    unit.body = code
    universe.currentRun.compileLate(unit)
  }
}
