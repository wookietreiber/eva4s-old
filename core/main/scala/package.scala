/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2012  Nils Foken, Christian Krause                                             *
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


package org

import scalaz.Id.Id

/** This package brings evolutionary algorithms to Scala.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define defaults contains default implementations.
  */
package object eva4s {

  // -----------------------------------------------------------------------------------------------
  // aliases
  // -----------------------------------------------------------------------------------------------

  /** Type alias for a pair of genomes. */
  type GenomeP[G] = Pair[G,G]

  /** Type alias for a pair of individuals. */
  type IndividualP[G] = Pair[Individual[G],Individual[G]]

  /** Returns Scala's default random object. */
  val Random = scala.util.Random

  /** A `Matchmaker` pairs individuals up with each other. It models parental selection.
    *
    * @tparam G $genome
    *
    * @see [[Matchmaking]] $defaults
    */
  type Matchmaker[G] = (Seq[Individual[G]],Int) ⇒ Seq[IndividualP[G]]

  /** A `Mutagen` determines the probability with which individuals mutate, depending on the current
    * generation.
    *
    * @see [[Mutagens]] $defaults
    */
  type Mutagen = Int ⇒ Double

  /** A `Selector` determines how the individuals for the next generation are chosen. It models
    * environmental selection.
    *
    * @tparam G $genome
    *
    * @see [[Selection]] $defaults
    */
  type Selector[G] = (Seq[Individual[G]],Seq[Individual[G]]) ⇒ Seq[Individual[G]]

  /** Recombination that per parent pair produces exactly one children. */
  type OnlyChildRecombination[G,P] = Recombination[G,P,Id]

  /** Standalone [[OnlyChildRecombination]] building block. */
  type OnlyChildRecombinator[G,P] = Recombinator[G,P,Id]

  /** Recombination that per parent pair produces exactly two children. */
  type CrossoverRecombination[G,P] = Recombination[G,P,GenomeP]

  /** Standalone [[CrossoverRecombination]] building block. */
  type CrossoverRecombinator[G,P] = Recombinator[G,P,GenomeP]

  // -----------------------------------------------------------------------------------------------
  // others
  // -----------------------------------------------------------------------------------------------

  /** Returns some debugger function. */
  val printer: Option[(Int,Double,Double) ⇒ Unit] = Some { (g: Int, i: Double, f: Double ) ⇒
    printf("generation: %5d     selection intensity: % 1.5f     average fitness: %f\n", g, i, f)
  }

  private[eva4s] def ranks(size: Int) = Vector.tabulate(size) {
    i ⇒ 2.0 / size * (1 - i.toDouble / (size - 1.0))
  }

}
