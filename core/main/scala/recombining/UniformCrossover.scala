package eva4s
package recombining

import language.higherKinds

import scala.util.Random

import scalaz.Functor
import scalaz.Unzip
import scalaz.Zip

/** Uniform crossover randomly distributes genes between the offspring based on a fixed mixing
  * ratio.
  *
  * @param mixingRatio Returns the mixing ratio.
  *
  * == Mixing Ratio ==
  *
  * The mixing ratio determines how the genes are distributed between the children, e.g.:
  *
  * - a value of zero will let the first child be equal to the first parent and the second child be
  *   equal to the second parent
  *
  * - a value of one will let the first child be equal to the second parent and the second child be
  *   equal to the first parent
  *
  * - a value of 0.5 (which is the default) will equally distribute the genes between the children,
  *   where each child will contain half of the genes of each parent
  */
case class UniformCrossover[F[_],A](mixingRatio: Double = 0.5)(implicit val fitness: Fitness[F[A]], F: Functor[F], U: Unzip[F], Z: Zip[F])
  extends CrossoverRecombinator[F[A]] {

  override def recombine(g1: F[A], g2: F[A]): GenomeP[F[A]] = {
    val cs = Z.zipWith(g1,g2) { (gene1,gene2) ⇒
      val s = Random.nextDouble

      val c1 = if (s >= mixingRatio) gene1 else gene2
      val c2 = if (s <  mixingRatio) gene1 else gene2

      (c1,c2)
    }

    U.unzip(cs)
  }

}
