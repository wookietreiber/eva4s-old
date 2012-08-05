/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2012  Nils Foken, Christian Krause                                             *
 *                                                                                               *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                                          *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>                                    *
 *                    <kizkizzbangbang@googlemail.com>                                           *
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

import math._

/** $MutagenInfo */
object Mutagens extends Mutagens

/** $MutagenInfo
  *
  * @define MutagenInfo Contains default [[org.eva4s.Mutagen]] implementations which determine the
  * mutation probability depending on the generation.
  *
  * The idiomatic usage of the functions defined here is to fill in the parameters of the first
  * parameter list(s) and use the remaining function as a [[org.eva4s.Mutagen]].
  *
  * @see [[org.eva4s.Mutagen]]
  *
  * @define start the mutation probability at generation zero
  * @define end the mutation probability at generation `generations`
  * @define generations the final generation
  * @define generation the current generation
  */
trait Mutagens {

  /** Returns a monotonically decreasing value based on `f(x) = a * exp(b*x)`.
    *
    * @param start $start
    * @param end $end
    * @param generations $generations
    * @param generation $generation
    */
  def ExponentialDecreasingMutagen(start: Double, end: Double)
                                  (generations: Int)
                                  (generation: Int): Double =
    start * pow(end / start, generation.toDouble / generations)

  /** Returns a monotonically decreasing value based on `f(x) = a + b*x`.
    *
    * @param start $start
    * @param end $end
    * @param generations $generations
    * @param generation $generation
    */
  def LinearDecreasingMutagen(start: Double, end: Double)
                             (generations: Int)
                             (generation: Int): Double =
    start + (end - start) / generations * generation

  /** Returns a `Mutagen` that always uses the same probability.
    *
    * @param probability the constant mutation probability
    */
  def ConstantMutagen(probability: Double): Mutagen = (_: Int) ⇒ probability

}
