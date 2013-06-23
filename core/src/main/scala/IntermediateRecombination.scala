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
import scalaz.Zip

/** Intermediate recombination randomly chooses offspring around and between the parents. It is
  * applicable to real number types only.
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
object IntermediateRecombination {

  /** Returns the default basis for the scaling factors. */
  def defaultScaling: Double = 0.25

  /** Returns a new genome by intermediate recombination.
    *
    * @tparam F gene container
    *
    * @param scaling basis for the scaling factors
    */
  def recombine[F[_]](scaling: Double = defaultScaling)(g1: F[Double], g2: F[Double])(implicit F: Functor[F], Z: Zip[F]): F[Double] = {
    Z.zipWith(g1,g2) { (gene1,gene2) ⇒
      val s = sample(scaling)
      gene1 * s + gene2 * (1 - s)
    }
  }

  private def sample(x: Double): Double = nextDoubleWithin(-x, 1 + x)

  private def nextDoubleWithin(lower: Double, upper: Double): Double =
    lower + (upper - lower) * Random.nextDouble

}
