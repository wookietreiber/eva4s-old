package eva4s
package matchmaking

import eva4s.util._

/** Returns a fixed amount of arbitrary pairs of individuals. This is the simplest form of
  * probabilistic matchmaking.
  */
case class RandomForcedMatchmaker[G]() extends Matchmaker[G] {

  override def apply(parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] =
    Vector.fill(pairs)(parents.choosePair)

}
