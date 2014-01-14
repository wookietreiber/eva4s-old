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
import scalaz.Length
import scalaz.Unzip
import scalaz.Zip

/** One point crossover randomly selects a crossover point and interchanges the two parents at this
  * point to produce two new children.
  */
object OnePointCrossover {

  /** Returns two new genomes by one point crossover.
    *
    * @tparam F gene container
    */
  def recombine[F[_],X](g1: F[X], g2: F[X])(implicit F: Functor[F], L: Length[F], U: Unzip[F], Z: Zip[F]): GenomeP[F[X]] = {
    val size = L.length(g1)

    val point = Random.nextInt(size - 1) + 1

    var current = 0

    val cs = Z.zipWith(g1,g2) { (gene1, gene2) ⇒
      val p = if (current < point) (gene1,gene2) else (gene2,gene1)
      current += 1
      p
    }

    U.unzip(cs)
  }

}
