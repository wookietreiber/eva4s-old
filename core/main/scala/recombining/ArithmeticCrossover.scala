package eva4s
package recombining

import language.higherKinds

import scalaz.Functor
import scalaz.Zip

/** Arithmetic crossover randomly chooses offspring around and between the parents. It is applicable
  * to real number types only.
  *
  * @param scaling Returns the scaling factor.
  *
  * == Scaling ==
  *
  * The scaling argument determines how much around the parents the offspring will be. A value of
  * zero will constrain the offspring to the same dimensions as the parents. The default scaling
  * value is chosen to be 0.25 because it allows the offspring to be a little around the parents,
  * thus to vary a little bit more.
  *
  * @todo abstract over value type, currently only `Double` (use spire)
  */
case class ArithmeticCrossover[F[_]](scaling: Double = 0.25)(implicit val fitness: Fitness[F[Double]], F: Functor[F], Z: Zip[F])
  extends CrossoverRecombinator[F[Double]] {

  override final def recombine(g1: F[Double], g2: F[Double]): GenomeP[F[Double]] = {
    val s = sample(scaling)

    val child1 = Z.zipWith(g1,g2) { (gene1,gene2) ⇒
      s * gene1 + (1 - s) * gene2
    }

    val child2 = Z.zipWith(g1,g2) { (gene1,gene2) ⇒
      (1 - s) * gene1 + s * gene2
    }

    (child1,child2)
  }

}
