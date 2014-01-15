package org.eva4s
package matchmaking

import scalay.collection._

import scalaz._
import Scalaz._

/** Probabilistic matchmaking. */
object MultipleTournamentMatchmaking extends DocDummy {

  /** Returns the fittest individuals of `pairs * parents` tournaments.
    *
    * There will be `pairs` tournaments to determine the pairs. Each tournament consists of
    * `parents` sub-tournaments. Each sub-tournament consists of `participants` randomly chosen
    * participants. The winner of a sub-tournament gets a point. The two individuals with the most
    * points are chosen. If, by chance, a single individual is the sole winner of all tournaments it
    * will pair up with the fittest individual.
    *
    * @tparam G $genome
    *
    * @param participants amount of randomly selected individuals attending a tournament
    * @param parents $parents
    * @param pairs $pairs
    */
  def apply[G](participants: Int)(parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] = {
    def winners = parents map { parent ⇒
      val ps = Seq(parent) ++ (parents filter { _ != parent } choose participants)
      Map(Pair(ps.minBy(_.fitness), 1))
    }

    Vector.fill(pairs) {
      val ps = winners.fold(Map())(_ ⊹ _).sortBy(- _._2).take(2).map(_._1)
      ps.size match {
        case 0 ⇒ parents.choosePair
        case 1 ⇒ Pair(ps.head, parents filter { _ != ps.head } minBy { _.fitness })
        case _ ⇒ Pair(ps.head, ps.last)
      }
    }
  }

}
