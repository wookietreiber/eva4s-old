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
