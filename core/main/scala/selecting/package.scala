package org.eva4s

/** Contains default [[Selector]] implementations which define environmental selection.
  *
  * The idiomatic usage is to input the parameters of the first parameter list(s) and use the
  * remaining function as a [[Selector]].
  */
package object selecting {

  /** A `Selector` determines how the individuals for the next generation are chosen. It models
    * environmental selection.
    *
    * @tparam G the type of the genome of the individuals, represents a solution of the problem
    */
  type Selector[G] = (Seq[Individual[G]],Seq[Individual[G]]) ⇒ Seq[Individual[G]]

  /** Dummy for documentation definition inheritance.
    *
    * @define genome the type of the genome of the individuals
    * @define mu the amount of chosen children
    * @define parents the parents of the generation
    * @define offspring the offspring of the generation
    */
  private[selecting] trait DocDummy

}
