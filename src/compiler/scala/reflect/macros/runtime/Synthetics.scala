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
    val path = if (file != null) file path else null
    pathFilter(if (path != null) path else "")
  }

  def existsAmongTrees(name: Name): Boolean = existsAtTopLevel(name)(path => path.endsWith(".scala"))

  def existsOnClassPath(name: Name): Boolean = existsAtTopLevel(name)(path => path.endsWith(".class"))

  def introduceTopLevel(packageName: String, definitions: ImplDef*): Unit = {
    def mkPid(fragments: List[String]): RefTree = fragments match {
      case hd :: Nil => Ident(TermName(hd))
      case hd :+ tl => Select(mkPid(hd), TermName(tl))
    }
    val pid = mkPid(packageName.split(java.util.regex.Pattern.quote(".")).toList)
    val code = PackageDef(pid, definitions.toList)
    // TODO: provide a way to specify a pretty name for debugging purposes
    val fakeJfile = enclosingUnit.source.file.file // compatibility with SBT
    val filesystemFile = new VirtualFile("macroSynthetic-" + randomUUID().toString.replace("-", "") + ".scala") { override def file = fakeJfile }
    val sourceFile = new BatchSourceFile(filesystemFile, code.toString)
    val unit = new CompilationUnit(sourceFile)
    unit.body = code
    universe.currentRun.compileLate(unit)
  }
}
