package scala.reflect
package api

import scala.language.implicitConversions

/** A slice of [[scala.reflect.api.Universe the Scala reflection cake]] that defines flag sets and operations on them.
 *  @see [[scala.reflect.api.Universe]] for a description of how the reflection API is encoded with the cake pattern.
 *
 *  Flags are used to provide modifiers for abstract syntax trees that represent definitions
 *  via the `flags` field of [[scala.reflect.api.Trees#Modifiers]]. Trees that accept modifiers are:
 *  [[scala.reflect.api.Trees#ClassDef]] (classes and traits), [[scala.reflect.api.Trees#ModuleDef]] (objects),
 *  [[scala.reflect.api.Trees#ValDef]] (vals, vars, parameters and self-type annotations),
 *  [[scala.reflect.api.Trees#DefDef]] (methods and constructors) and
 *  [[scala.reflect.api.Trees#TypeDef]] (type aliases, abstract type members and type parameters).
 *
 *  For example, to create a class named `C` one would write `ClassDef(Modifiers(NoFlags), newTypeName("C"), Nil, ...)`.
 *  Here the flag set is empty, representing a vanilla class definition. To make `C` private, one would write
 *  `ClassDef(Modifiers(PRIVATE), newTypeName("C"), Nil, ...)`. Flags can also be combined with the vertical bar operator (`|`).
 *  For example, a private final class is written as followed: `ClassDef(Modifiers(PRIVATE | FINAL), newTypeName("C"), Nil, ...)`.
 *
 *  The list of all available flags is defined in [[scala.reflect.api.FlagSets#FlagValues]], available via the [[scala.reflect.api.FlagSets#Flag]]
 *  (typically one writes a blanket import for that, e.g. `import scala.reflect.api.universe.Flag._`).
 *
 *  Definition trees are compiled down to symbols, so flags on modifiers of such trees are transformed into flags on the resulting symbols.
 *  Unlike trees, symbols don't expose flags, but rather provide `isXXX` test methods (e.g. `isFinal` can be used to test finality). These test methods
 *  might require an upcast with `asTerm`, `asType` or `asClass` as some flags only make sense for certain flavors of symbols.
 *
 *  Known issue: this API is considered to be a candidate for redesign. It is quite probable that in future releases of the reflection API
 *  flag sets will be replaced with something else.
 */
trait FlagSets { self: Universe =>

  /** An abstract type representing sets of flags (like private, final, etc.) that apply to definition trees and symbols */
  type FlagSet

  /** A tag that preserves the identity of the `FlagSet` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val FlagSetTag: ClassTag[FlagSet]

  /** The API of `FlagSet` instances.
   *  The main source of information about flag sets is the [[scala.reflect.api.FlagSets]] page.
   */
  trait FlagOps extends Any {
    /** Produces a flag set that's a union of this flag set and the provided flag set. */
    def | (right: FlagSet): FlagSet
  }

  /** The API of `FlagSet` instances. */
  implicit def addFlagOps(left: FlagSet): FlagOps

  /** A module that contains all possible values that can constitute flag sets. */
  val Flag: FlagValues

  // Q: I have a pretty flag. Can I put it here?
  // A: Only if there's a tree that cannot be built without it.
  //    If you want to put a flag here so that it can be tested against,
  //    introduce an `isXXX` method in one of the `api.Symbols` classes instead.

  /** All possible values that can constitute flag sets.
   *  The main source of information about flag sets is the [[scala.reflect.api.FlagSets]] page.
   */
  trait FlagValues {

    /** Flag indicating that tree represents a trait */
    val TRAIT: FlagSet

    /** Flag indicating that a tree is an interface (i.e. a trait which defines only abstract methods) */
    val INTERFACE: FlagSet

    /** Flag indicating that tree represents a mutable variable */
    val MUTABLE: FlagSet

    /** Flag indicating that tree represents a macro definition. */
    val MACRO: FlagSet

    /** Flag indicating that tree represents an abstract type, method, or value */
    val DEFERRED: FlagSet

    /** Flag indicating that tree represents an abstract class */
    val ABSTRACT: FlagSet

    /** Flag indicating that tree has `final` modifier set */
    val FINAL: FlagSet

    /** Flag indicating that tree has `sealed` modifier set */
    val SEALED: FlagSet

    /** Flag indicating that tree has `implicit` modifier set */
    val IMPLICIT: FlagSet

    /** Flag indicating that tree has `lazy` modifier set */
    val LAZY: FlagSet

    /** Flag indicating that tree has `override` modifier set */
    val OVERRIDE: FlagSet

    /** Flag indicating that tree has `private` modifier set */
    val PRIVATE: FlagSet

    /** Flag indicating that tree has `protected` modifier set */
    val PROTECTED: FlagSet

    /** Flag indicating that tree represents a member local to current class
     *  (i.e. private[this] or protected[this].
     *  This requires having either PRIVATE or PROTECTED set as well.
     */
    val LOCAL: FlagSet

    /** Flag indicating that tree has `case` modifier set */
    val CASE: FlagSet

    /** Flag indicating that tree has `abstract` and `override` modifiers set */
    val ABSOVERRIDE: FlagSet

    /** Flag indicating that tree represents a by-name parameter */
    val BYNAMEPARAM: FlagSet

    /** Flag indicating that tree represents a class or parameter.
     *  Both type and value parameters carry the flag. */
    val PARAM: FlagSet

    /** Flag indicating that tree represents a covariant
     *  type parameter (marked with `+`). */
    val COVARIANT: FlagSet

    /** Flag indicating that tree represents a contravariant
     *  type parameter (marked with `-`). */
    val CONTRAVARIANT: FlagSet

    /** Flag indicating that tree represents a parameter that has a default value */
    val DEFAULTPARAM: FlagSet

    /** Flag indicating that tree represents an early definition */
    val PRESUPER: FlagSet

    /** Flag indicating that tree represents a variable or a member initialized to the default value */
    val DEFAULTINIT: FlagSet
  }

  /** The empty set of flags */
  val NoFlags: FlagSet
}
