package org.eva4s
package recombining

import language.higherKinds

import scala.util.Random

import scalaz.Functor
import scalaz.Zip

/** Arithmetic crossover randomly chooses offspring around and between the parents. It is applicable
  * to real number types only.
  *
  * == Scaling ==
  *
  * The scaling argument determines how much around the parents the offspring will be. A value of
  * zero will constrain the offspring to the same dimensions as the parents. The default scaling
  * value is chosen to be 0.25 because it allows the offspring to be a little around the parents,
  * thus to vary a little bit more.
  *
  * @todo abstract over value type, currently only `Double`
  */
object ArithmeticCrossover {

  /** Returns the default basis for the scaling factor. */
  def defaultScaling: Double = 0.25

  /** Returns a new genome by arithmetic crossover.
    *
    * @tparam F gene container
    *
    * @param scaling basis for the scaling factors
    */
  def recombine[F[_]](scaling: Double = defaultScaling)(g1: F[Double], g2: F[Double])(implicit F: Functor[F], Z: Zip[F]): GenomeP[F[Double]] = {
    val s = sample(scaling)

    val child1 = Z.zipWith(g1,g2) { (gene1,gene2) ⇒
      s * gene1 + (1 - s) * gene2
    }

    val child2 = Z.zipWith(g1,g2) { (gene1,gene2) ⇒
      (1 - s) * gene1 + s * gene2
    }

    (child1,child2)
  }

  private def sample(x: Double): Double = nextDoubleWithin(-x, 1 + x)

  private def nextDoubleWithin(lower: Double, upper: Double): Double =
    lower + (upper - lower) * Random.nextDouble

}
