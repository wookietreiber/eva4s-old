package org.eva4s
package recombining

import language.higherKinds

import scalay.collection._

import scalaz.Functor
import scalaz.Length
import scalaz.Unzip
import scalaz.Zip

/** Two point crossover randomly selects two crossover points and interchanges the two parents at
  * these points to produce two new children.
  */
object TwoPointCrossover {

  /** Returns two new genomes by two point crossover.
    *
    * @tparam F gene container
    */
  def recombine[F[_],X](g1: F[X], g2: F[X])(implicit F: Functor[F], L: Length[F], U: Unzip[F], Z: Zip[F]): GenomeP[F[X]] = {
    val size = L.length(g1)

    val points = (1 to (size-1)).choose(2).sorted

    val p1 = points(0)
    val p2 = points(1)

    var current = 0

    val cs = Z.zipWith(g1,g2) { (gene1, gene2) ⇒
      val p = if (current < p1 || current >= p2) (gene1,gene2) else (gene2,gene1)
      current += 1
      p
    }

    U.unzip(cs)
  }

}
