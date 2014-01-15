package org.eva4s

/** Contains default [[Matchmaker]] implementations which define parental selection.
  *
  * The idiomatic usage is to input the parameters of the first parameter list(s) and use the
  * remaining function as a [[Matchmaker]].
  */
package object matchmaking {

  /** A `Matchmaker` pairs individuals up with each other. It models parental selection.
    *
    * @tparam G the type of the genome of the individuals, represents a solution of the problem
    */
  type Matchmaker[G] = (Seq[Individual[G]],Int) ⇒ Seq[IndividualP[G]]

  /** Dummy for documentation definition inheritance.
    *
    * @define acceptance chance of a pair accepting the match
    * @define genome the type of the genome of the individuals
    * @define pairs the amount of pairs generated
    * @define parents the parents of the generation
    */
  private[matchmaking] trait DocDummy

}
