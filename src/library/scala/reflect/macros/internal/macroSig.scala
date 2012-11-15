package scala.reflect.macros
package internal

/** Links non-def macros to their signatures.
 *
 *  The necessity for this annotation has arisen when I started writing type macros.
 *  First of all we cannot represent type macros as TypeDefs, because those AST nodes lack vparamss and tpt.
 *  On the other hand we had an idea of representing those as DefDefs which have TypeNames.
 *  But that failed as well, because then it's unclear what symbols should these DefDefs have.
 *  MethodSymbols won't do and TypeSymbols on a DefDef, well, this doesn't sound good as well.
 */
private[scala] class macroSig(val sig: Any) extends scala.annotation.StaticAnnotation
