/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2012  Nils Foken, Christian Krause                                             *
 *                2013  Christian Krause                                                         *
 *                                                                                               *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                                          *
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

import math._

/** $MutagenInfo */
object Mutagens extends Mutagens

/** $MutagenInfo
  *
  * @define MutagenInfo Contains default [[Mutagen]] implementations which determine the mutation
  * probability depending on the generation.
  *
  * @see [[Mutagen]]
  *
  * @define start the mutation probability at generation zero
  * @define end the mutation probability at generation `generations`
  * @define generations the final generation
  */
trait Mutagens {

  /** Returns a monotonic function based on `f(x) = a * exp(b*x)`.
    *
    * @param start $start
    * @param end $end
    * @param generations $generations
    */
  def ExponentialMutagen(generations: Int,
                         start: Double = 0.8,
                         end: Double = 0.01)
                         : Mutagen = new Function[Int,Double] {
    val a = start
    val c = end / start

    override def apply(generation: Int): Double = a * pow(c, generation.toDouble / generations)

    override def toString = "ExponentialMutagen(start=%s,end=%s,generations=%s)".format(
      start, end, generations
    )
  }

  /** Returns a monotonic function based on `f(x) = a + b * pow(x,degree)`.
    *
    * @param degree the degree of the polynom
    * @param start $start
    * @param end $end
    * @param generations $generations
    */
  def PolynomialMutagen(degree: Double,
                        generations: Int,
                        start: Double = 0.8,
                        end: Double = 0.01)
                        : Mutagen = new Function[Int,Double] {
    val a = start
    val b = ((end - start) / pow(generations, degree))

    override def apply(generation: Int): Double = a + b * pow(generation, degree)

    override def toString = degree match {
      case 1.0 ⇒ "LinearMutagen(start=%s,end=%s,generations=%s)".format(
        start, end, generations
      )
      case 2.0 ⇒ "QuadraticMutagen(start=%s,end=%s,generations=%s)".format(
        start, end, generations
      )
      case 3.0 ⇒ "CubicMutagen(start=%s,end=%s,generations=%s)".format(
        start, end, generations
      )
      case degree ⇒ "PolynomialMutagen(degree=%s,start=%s,end=%s,generations=%s)".format(
        degree, start, end, generations
      )
    }
  }

  /** Returns a `Mutagen` that always uses the same probability.
    *
    * @param probability the constant mutation probability
    */
  def ConstantMutagen(probability: Double): Mutagen = new Function[Int,Double] {
    override def apply(generation: Int): Double = probability

    override def toString = "ConstantMutagen(probability=%s)".format(probability)
  }

}
