package org.eva4s
package selecting

/** Deterministic selection. */
object PlusSelection extends DocDummy {

  /** Returns the fittest individuals.
    *
    * Plus selection (aka (μ+λ) selection) represents an elitist selection, which deterministically
    * chooses the best μ individuals from all individuals (`parents ++ offspring`, i.e. `μ+λ`).
    *
    * @tparam G $genome
    *
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring, determines `λ = offspring.size`
    */
  def apply[G](parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    (parents ++ offspring) sortBy { _.fitness } take parents.size

}
