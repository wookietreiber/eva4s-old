package org.eva4s
package recombining

import language.higherKinds

import scala.util.Random

import scalaz.Functor
import scalaz.Unzip
import scalaz.Zip

/** Uniform crossover randomly distributes genes between the offspring based on a fixed mixing
  * ratio.
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
object UniformCrossover {

  /** Returns the default mixing ratio for the crossover. */
  def defaultMixingRatio: Double = 0.5

  /** Returns two new genomes by uniform crossover.
    *
    * @tparam F gene container
    *
    * @param mixingRatio determines gene distribution between the children
    */
  def recombine[F[_],A](mixingRatio: Double = defaultMixingRatio)(g1: F[A], g2: F[A])(implicit F: Functor[F], U: Unzip[F], Z: Zip[F]): GenomeP[F[A]] = {
    val cs = Z.zipWith(g1,g2) { (gene1,gene2) ⇒
      val s = Random.nextDouble

      val c1 = if (s >= mixingRatio) gene1 else gene2
      val c2 = if (s <  mixingRatio) gene1 else gene2

      (c1,c2)
    }

    U.unzip(cs)
  }

}
