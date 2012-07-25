/* **************************************************************************
 *                                                                          *
 *  Copyright (C)  2012  Nils Foken, Christian Krause                       *
 *                                                                          *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                     *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>               *
 *                                                                          *
 ****************************************************************************
 *                                                                          *
 *  This file is part of 'eva4s'.                                           *
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


package org

import scala.collection.GenTraversable

/** This package brings evolutionary algorithms to Scala.
  *
  * @define genome the type of the genome of the individuals
  * @define defaults contains default implementations.
  */
package object eva4s {

  /** Represents an individual with its genome and fitness.
    *
    * @tparam G the type of the genome of the individuals
    *
    * @param genome Returns the genome of this individual.
    * @param fitness Returns the fitness of this individual.
    *
    * @note [[org.eva4s.Evolutionary]] provides more convenient methods for creating individuals.
    */
  case class Individual[G](genome: G, fitness: Double)

  // -----------------------------------------------------------------------------------------------
  // aliases
  // -----------------------------------------------------------------------------------------------

  /** Returns Scala's default [[scala.util.Random]] object. */
  val Random = scala.util.Random

  /** A `Matchmaker` pairs individuals up with each other. It models parental selection.
    *
    * @tparam G $genome
    *
    * @see [[org.eva4s.Matchmaking]] $defaults
    */
  type Matchmaker[G] = Iterable[Individual[G]] ⇒ Iterable[Pair[Individual[G],Individual[G]]]

  /** A `Mutagen` determines the probability with which individuals mutate, depending on the current
    * generation.
    *
    * @see [[org.eva4s.Mutagens]] $defaults
    */
  type Mutagen = Int ⇒ Double

  /** A `Selector` determines how the individuals for the next generation are chosen. It models
    * environmental selection.
    *
    * @tparam G $genome
    *
    * @see [[org.eva4s.Selection]] $defaults
    */
  type Selector[G] = (Iterable[Individual[G]], Iterable[Individual[G]]) ⇒ Iterable[Individual[G]]

  // -----------------------------------------------------------------------------------------------
  // others
  // -----------------------------------------------------------------------------------------------

  /** Returns some debugger function. */
  val printer: Option[(Int,Double,Double) ⇒ Unit] = Some { (g: Int, i: Double, f: Double) ⇒
    printf("generation: %5d   intensity: % 10.6f   average fitness: %f\n", g, i, f)
  }

  private[eva4s] def ranks(size: Int) = Vector.tabulate(size) {
    i ⇒ 2.0 / size * (1 - i.toDouble / (size - 1.0))
  }

}
