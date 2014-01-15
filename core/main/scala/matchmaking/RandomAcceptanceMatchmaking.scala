package org.eva4s
package matchmaking

import scala.util.Random

import scalay.collection._

/** Probabilistic matchmaking. */
object RandomAcceptanceMatchmaking extends DocDummy {

  /** Returns a varying amount of arbitrary pairs of individuals.
    *
    * @tparam G $genome
    *
    * @param acceptance $acceptance
    * @param parents $parents
    * @param pairs $pairs
    */
  def apply[G](acceptance: Double)(parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] =
    for (i ← 1 to pairs if Random.nextDouble < acceptance) yield parents.choosePair

}
