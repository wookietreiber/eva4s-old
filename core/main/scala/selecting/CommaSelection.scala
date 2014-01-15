package org.eva4s
package selecting

/** Deterministic selection. */
object CommaSelection extends DocDummy {

  /** Returns the fittest offspring.
    *
    * Comma selection (aka (μ,λ) selection) represents an elitist selection, which deterministically
    * chooses the best μ individuals (`survivors`) from the offspring of size λ. As smaller as the
    * `μ/λ` ratio gets, the higher is the ''selection pressure'' (aka ''evolutionary pressure'').
    * Recommended is to choose `μ` in a way so that `1/7 ≤ μ/λ ≤ 1/5`, which is done by default.
    *
    * '''Note:''' The parents (the survivors of the previous generation) are not considered with
    * this kind of selection.
    *
    * @tparam G $genome
    *
    * @param survivors $mu, default is `μ = λ/6`
    * @param parents $parents
    * @param offspring $offspring, determines `λ = offspring.size`
    */
  def apply[G](survivors: Int ⇒ Int = (λ: Int) ⇒ math.round(λ.toFloat / 6))
              (parents: Seq[Individual[G]], offspring: Seq[Individual[G]])
               : Seq[Individual[G]] =
    offspring sortBy { _.fitness } take survivors(offspring.size)

}
