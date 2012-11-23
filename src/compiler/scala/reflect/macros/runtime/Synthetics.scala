/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.macros
package runtime

import java.util.UUID._
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualFile

trait Synthetics {
  self: Context =>

  def introduceTopLevel(code: Tree): Tree = {
    // TODO: provide a way to specify a pretty name for debugging purposes
    val filesystemFile = new VirtualFile("/macroSynthetics/" + randomUUID())
    val sourceFile = new BatchSourceFile(filesystemFile, code.toString)
    val unit = new CompilationUnit(sourceFile)
    def wrap(code: Tree): Tree = code match {
      case PackageDef(_, _) => code
      case ClassDef(_, _, _, _) => wrap(Block(List(code), Literal(Constant(()))))
      case ModuleDef(_, _, _) => wrap(Block(List(code), Literal(Constant(()))))
      case Block(cdefs, _) => PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), cdefs)
    }
    unit.code = wrap(code)
    universe.currentRun.compileLate(unit)
  }
}
