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
  */
trait Mutagens {

  /** Returns a monotonically decreasing value based on `f(x) = a * exp(b*x)`.
    *
    * There are two points defined from which `a` and `b` derive:
    *
    *   1. `(0,startProbability)`
    *
    *   2. `(generations, endProbability)`
    *
    * @param startProbability the mutation probability at generation zero
    * @param endProbability the mutation probability at generation `generations`
    * @param generations the final generation
    * @param generation the current generation
    */
  def ExponentialMutagen(startProbability: Double,
                         endProbability: Double)
                        (generations: Int)
                        (generation: Int): Double = {
    startProbability * exp(log(endProbability / startProbability) / generations * generation)
  }

}
