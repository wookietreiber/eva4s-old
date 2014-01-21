package eva4s
package selecting

/** A deterministic [[Selector]] which chooses the fittest individuals.
  *
  * Plus selection (aka (μ+λ) selection) represents an elitist selection, which deterministically
  * chooses the best μ individuals from all individuals (`parents ++ offspring`, i.e. `μ+λ`).
  */
case class PlusSelector[G]() extends Selector[G] {

  override def apply(parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    (parents ++ offspring).sortBy(_.fitness).take(parents.size)

}
