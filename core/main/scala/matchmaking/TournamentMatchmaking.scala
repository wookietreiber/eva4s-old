package org.eva4s
package matchmaking

import scalay.collection._

/** Probabilistic matchmaking. */
object TournamentMatchmaking extends DocDummy {

  /** Returns the fittest individuals of `pairs` tournaments.
    *
    * There will be `pairs` tournaments to determine the pairs. Each tournament consists of
    * `participants` randomly chosen participants. From these participants are the fittest two for
    * the pair chosen.
    *
    * @tparam G $genome
    *
    * @param participants amount of randomly selected individuals attending a tournament
    * @param parents $parents
    * @param pairs $pairs
    */
  def apply[G](participants: Int)(parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] = {
    require(participants >= 2, "participants must be greater or equal to 2")

    Vector.fill(pairs) {
      val winners = parents choose participants sortBy { _.fitness } take 2
      Pair(winners.head, winners.last)
    }
  }

}
