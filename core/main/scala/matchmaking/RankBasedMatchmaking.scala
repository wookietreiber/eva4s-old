package org.eva4s
package matchmaking

import scala.util.Random

/** Probabilistic matchmaking. */
object RankBasedMatchmaking extends DocDummy {

  /** Returns pairs based on their rank by fitness. The fitter the individual, the higher is the
    * possibility that it gets chosen.
    *
    * @tparam G $genome
    *
    * @param parents $parents
    * @param pairs $pairs
    *
    * @todo needs performance improvement
    */
  def apply[G](parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] = {
    val ranked = parents sortBy { - _.fitness } zip {
      ranks(parents.size).inits.drop(1).map(_.sum).toList
    }

    def choosePair(ranked: Seq[Pair[Individual[G],Double]]): IndividualP[G] = {
      // improve this to not use partition
      val r1 = Random.nextDouble
      val (p11,p12) = ranked partition { _._2 < r1 }
      val par1 = p11.last._1

      val r2 = Random.nextDouble
      val (p21,p22) = (p11.init ++ p12) partition { _._2 < r2 }
      val par2 = if (p21.isEmpty) p22.head._1 else p21.last._1

      par1 → par2
    }

    Vector.fill(pairs) { choosePair(ranked) }
  }

}
