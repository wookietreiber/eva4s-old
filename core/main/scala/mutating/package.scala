package org.eva4s

/** Contains default [[Mutagen]] implementations which determine the mutation probability depending
  * on the generation.
  *
  * The idiomatic usage is to input the parameters of the first parameter list(s) and use the
  * remaining function as a [[Mutagen]].
  */
package object mutating {

  /** A `Mutagen` determines the probability with which individuals mutate, depending on the current generation. */
  type Mutagen = Int ⇒ Double

  /** Dummy for documentation definition inheritance.
    *
    * @define start the mutation probability at generation zero
    * @define end the mutation probability at generation `generations`
    * @define generations the final generation
    */
  private[mutating] trait DocDummy

}
