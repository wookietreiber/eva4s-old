/* **************************************************************************
 *                                                                          *
 *  Copyright (C)  2012  Christian Krause                                   *
 *                                                                          *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>               *
 *                                                                          *
 ****************************************************************************
 *                                                                          *
 *  This file is part of 'scalevalgo'.                                      *
 *                                                                          *
 *  This project is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by    *
 *  the Free Software Foundation, either version 3 of the License, or       *
 *  any later version.                                                      *
 *                                                                          *
 *  This project is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 *  GNU General Public License for more details.                            *
 *                                                                          *
 *  You should have received a copy of the GNU General Public License       *
 *  along with this project. If not, see <http://www.gnu.org/licenses/>.    *
 *                                                                          *
 ****************************************************************************/


package ea

import math._

/** $mutageninfo */
object Mutagens extends Mutagens

/** $mutageninfo
  *
  * @define mutageninfo Contains default [[ea.Mutagen]] implementations which determine the mutation
  * probability depending on the generation.
  *
  * The idiomatic usage of the functions defined here is to fill in the parameters of the first
  * parameter list(s) and use the remaining function as a [[ea.Mutagen]].
  *
  * @see [[ea.Mutagen]]
  *
  * @define startProbability the mutation probability at generation zero
  * @define endProbability the mutation probability at generation `generations`
  * @define generations the final generation
  * @define generation the current generation
  */
trait Mutagens {

  /** Returns a monotonically decreasing value based on `f(x) = a * exp(b*x)`.
    *
    * @param startProbability $startProbability
    * @param endProbability $endProbability
    * @param generations $generations
    * @param generation $generation
    */
  def ExponentialMutagen(startProbability: Double,
                         endProbability: Double)
                        (generations: Int)
                        (generation: Int): Double = {
    startProbability * pow(endProbability / startProbability, generations / generation)
  }

  /** Returns a monotonically decreasing value based on `f(x) = a + b*x`.
    *
    * @param startProbability $startProbability
    * @param endProbability $endProbability
    * @param generations $generations
    * @param generation $generation
    */
  def LinearMutagen(startProbability: Double,
                    endProbability: Double)
                   (generations: Int)
                   (generation: Int): Double = {
    startProbability + (endProbability - startProbability) / generations * generation
  }

  /** Returns a `Mutagen` that always uses the same probability.
    *
    * @param probability the constant mutation probability
    */
  def ConstantMutagen(probability: Double): Mutagen = (_: Int) ⇒ probability

}
