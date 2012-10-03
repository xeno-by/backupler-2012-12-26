package scala.reflect

import scala.reflect.api.{Universe => ApiUniverse}

/** The Scala reflection API.
 *
 *  === Universes ===
 *
 *  Standard reflection interfaces and implementations are all contained in the package scala-reflect.jar.
 *  This jar is needed for all operations involving either Java reflection or macro implementations.
 *  The two share a large set of operations, which are all abstracted out in the reflective core API in [[scala.reflect.api.Universe]].
 *  This universe provides a fairly complete set of reflection operations that allow to query key Scala type relations such as membership or subtyping.
 *
 *  [[scala.reflect.api.Universe]] has two specialized sub-universes. [[scala.reflect.api.JavaUniverse]] adds operations that link symbols and types
 *  to the underlying classes and runtime values of a JVM. [[scala.reflect.macros.Universe]] adds operations which allow macros to access selected
 *  compiler data structures and operations.
 *
 *  The main implementation object of scala-reflect.jar is named [[scala.reflect.runtime.package#universe scala.reflect.runtime.universe]].
 *  It is a global singleton, which serves as an entry point to runtime reflection.
 *  There is no analogous global singleton universe for macros. Instead, macros access the currently running compiler instance as their universe,
 *  accessible via [[scala.reflect.macros.Context#universe]].
 *
 *  === Mirrors ===
 *
 *  Each universe has one or more mirrors. A mirror defines a hierarchy of symbols starting with the root package
 *  `_root_` and provides methods to locate and define classes and singleton objects in that hierarchy.
 *  Mirrors for runtime reflection also provide operations to reflect on runtime instances.
 *
 *  All universes have one root mirror each, available in the `rootMirror` field.
 *  This mirror contains standard Scala classes and types
 *  such as `Any`, `AnyRef`, `AnyVal`, `Nothing`, `Null`, and all classes loaded from scala-library.
 *  The root package of the root mirror contains the root packages of all other mirrors as members.
 *
 *  In a Java universe each mirror is associated with a classloader. This reflects the fact that multiple classes
 *  with the same name can exist in a JVM instance, where each class is loaded by a different classloader.
 *  To model this behavior, each JVM classloader is associated with a mirror, and each mirror contains its own
 *  hierarchy of packages and classes. However, the same class may also exist in several different classloaders
 *  and mirrors because classloaders can delegate to each other. This is modelled by one level of indirection:
 *  several packages in different mirrors can link to the same class.
 *
 *  The main access point to mirrors in runtime reflection is [[scala.reflect.runtime.package#currentMirror]],
 *  which gives a JVM reflection mirror that corresponds to the current lexical context.
 *  `currentMirror` is typically equivalent to `universe.runtimeMirror(getClass.getClassLoader)` invoked at the call site.
 *  Macro universe is not based on classloaders, therefore it has only one mirror that corresponds to the compiler classpath,
 *  accessible via [[scala.reflect.macros.Context#mirror]].
 *
 *  === Toolboxes ===
 *
 *  Along with runtime Java universe [[scala.reflect.api.Universe]] and compile-time macro universe [[scala.reflect.macros.Universe]],
 *  reflection API also includes a runtime compiler universe implemented in `scala.tools.reflect`. One interacts with such universes
 *  via toolboxes, instances of `scala.tools.reflect.ToolBox` declared in scala-compiler.jar.
 *
 *  After importing the `scala.tools.reflect.ToolBox` implicit conversion, runtime reflection mirrors gain the `mkToolBox` method
 *  that lets one create runtime compiler instances, optionally providing custom `options` string and a custom `frontEnd` that determines
 *  how to process warnings and errors emitted by the compilers. Toolboxes have such methods as `parse`, `typeCheck`, `inferImplicitValue`, `compile` and `eval`.
 *
 *  === Known issues ===
 *
 *  In Scala 2.10.0, reflection API and its implementation have experimental status. This means that the API and the docs are not complete and can be changed
 *  in binary- and source-incompatible manner in 2.10.1. This also means that the implementation has known issues. Here are some useful links:
 *    - [[https://issues.scala-lang.org/secure/IssueNavigator.jspa?mode=hide&requestId=10908 Known issues in reflection and macros]]
 *    - [[http://stackoverflow.com/questions/tagged/scala+reflection Questions tagged "scala" and "reflection" at Stack Overflow]]
 *
 *  === Examples ===
 *
 *  This page focuses on typical tasks that are performed with runtime reflection: getting a class or an object, loading and inspecting its members
 *  and invoking them at runtime.
 *
 *  However there's much more to Scala reflection, with examples on other documentation pages answering the following questions:
 *    - [[scala.reflect.api.Symbols How to get a Symbol that corresponds to a given definition?]]
 *    - [[scala.reflect.api.Types How to get a Type of some Scala code?]]
 *    - [[scala.reflect.api.Trees How to get a Tree that corresponds to some Scala code?]]
 *    - [[scala.reflect.api.Trees How to parse a string into a Tree?]]
 *    - [[scala.reflect.api.Trees How to compile or evaluate a Tree?]]
 *    - [[scala.reflect.api.Annotations How to get Java and/or Scala annotations attached to a given definition?]]
 *    - [[scala.reflect.api.Printers How to inspect internal structure of reflection artifacts?]]
 *    - [[scala.reflect.api.Importers How to move reflection artifacts from one universe to another?]]
 *    - [[scala.reflect.macros.package How to use compile-time reflection in macros?]]
 */
package object api {

  // anchors for materialization macros emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove these anchors
  private[scala] def materializeWeakTypeTag[T](u: ApiUniverse): u.WeakTypeTag[T] = ??? // macro
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = ??? // macro
}