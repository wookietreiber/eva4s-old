package eva4s
package selecting

/** A deterministic, elitist [[Selector]] which chooses the fittest offspring.
  *
  * Comma selection (aka (μ,λ) selection) represents an elitist selection, which deterministically
  * chooses the best μ individuals (`survivors`) from the offspring of size λ. As smaller as the
  * `μ/λ` ratio gets, the higher is the ''selection pressure'' (aka ''evolutionary pressure'').
  * Recommended is to choose `μ` in a way so that `1/7 ≤ μ/λ ≤ 1/5`, which is done by default via
  * the rate 6, i.e. `μ/λ = 1/6`.
  *
  * @param survivors function from size of offspring to the amount of it that is chosen
  */
case class CommaSelector[G](survivors: Int => Int) extends Selector[G] {

  override def apply(parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    offspring.sortBy(_.fitness).take(survivors(offspring.size))

}

/** [[CommaSelector]] factory. */
object CommaSelector {

  /** Returns a new [[CommaSelector]].
    *
    * @param rate rate of offspring that is chosen for the next generation, must be greater than 1
    */
  def apply[G](rate: Double = 6.0): Selector[G] = {
    require(rate > 1.0)

    apply((λ: Int) => (λ / rate).round.toInt)
  }

}
