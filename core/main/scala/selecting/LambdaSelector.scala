package eva4s
package selecting

/** A deterministic [[Selector]] which chooses the entire offspring. */
case class LambdaSelector[G]() extends Selector[G] {

  override def apply(parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    offspring

}
