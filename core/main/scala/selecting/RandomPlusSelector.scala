package eva4s
package selecting

import eva4s.util._

/** A probabilistic [[Selector]] which chooses individuals arbitrarily from both `parents` and
  * `offspring` generation. This is the simplest form of probabilistic selection.
  */
case class RandomPlusSelector[G]() extends Selector[G] {

  override def apply(parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    (parents ++ offspring) choose parents.size

}
