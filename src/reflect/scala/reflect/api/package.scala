package scala.reflect

import scala.reflect.api.{Universe => ApiUniverse}

package object api {

  // anchors for materialization macros emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove these anchors
  private[scala] def materializeWeakTypeTag[T](u: ApiUniverse): u.WeakTypeTag[T] = ??? // macro
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = ??? // macro

  /** The entry point into runtime reflection.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use it.
   */
  lazy val universe: scala.reflect.api.JavaUniverse = scala.reflect.runtime.universe

  /** The runtime reflection mirror that corresponds to the current lexical context.
   *  Is typically equivalent to `universe.runtimeMirror(getClass.getClassLoader)` invoked at the call site.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use it.
   */
  def currentMirror: universe.Mirror = ???
}