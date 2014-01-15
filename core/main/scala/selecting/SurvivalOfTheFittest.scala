package org.eva4s
package selecting

/** Deterministic selection. */
object SurvivalOfTheFittest extends DocDummy {

  /** Returns the fittest individuals. This is an alias for [[PlusSelection]].
    *
    * @tparam G $genome
    *
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring, determines `λ = offspring.size`
    *
    * @see [[selecting.PlusSelection]]
    */
  def apply[G](parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    PlusSelection(parents,offspring)

}
