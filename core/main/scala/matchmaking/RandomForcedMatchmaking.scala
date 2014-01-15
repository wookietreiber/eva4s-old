package org.eva4s
package matchmaking

import scalay.collection._

/** Probabilistic matchmaking. */
object RandomForcedMatchmaking extends DocDummy {

  /** Returns a fixed amount of arbitrary pairs of individuals. This is the simplest form of
    * probabilistic matchmaking.
    *
    * @tparam G $genome
    *
    * @param parents $parents
    * @param pairs $pairs
    */
  def apply[G](parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] =
    Vector.fill(pairs) { parents.choosePair }

}
