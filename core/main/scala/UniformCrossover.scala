/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2013  Christian Krause                                                         *
 *                                                                                               *
 *  Christian Krause  <kizkizzbangbang@googlemail.com>                                           *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  This file is part of 'eva4s'.                                                                *
 *                                                                                               *
 *  This project is free software: you can redistribute it and/or modify it under the terms      *
 *  of the GNU General Public License as published by the Free Software Foundation, either       *
 *  version 3 of the License, or any later version.                                              *
 *                                                                                               *
 *  This project is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;    *
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    *
 *  See the GNU General Public License for more details.                                         *
 *                                                                                               *
 *  You should have received a copy of the GNU General Public License along with this project.   *
 *  If not, see <http://www.gnu.org/licenses/>.                                                  *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


package org.eva4s

import language.higherKinds

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
