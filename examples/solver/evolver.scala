/* **************************************************************************
 *                                                                          *
 *  Copyright (C)  2012  Nils Foken, Christian Krause                       *
 *                                                                          *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                     *
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


package org.eva4s
package solver

import scala.annotation.tailrec
import scalay.collection._

import Evolvers._

object SplitEvolver extends Evolver {
  def apply[G,P](ea: Evolutionary[G,P])
                (generations: Int = 200,
                 survivors: Int = 100)
                (implicit matchmaker: Matchmaker[G] = RankBasedMatchmaker[G] _,
                          mutagen: Mutagen = ExponentialDecreasingMutagen(0.8, 0.1)(generations),
                          selector: Selector[G] = ChildSelection[G] _,
                          debugger: Option[(Int,Double,Double) ⇒ Unit] = None)
                 : Individual[G] = {
    import ea._

    @tailrec
    def evolve(parents: Iterable[Individual[G]], generation: Int): Individual[G] =
      if (generation == generations) {
        parents minBy { _.fitness }
      } else {
        val recombinations = (survivors * (1. - mutagen(generation)) / 2.).round.toInt
        val mutations      = survivors - (2 * recombinations)

        val mutants   = parents choose mutations map Mutant
        val offspring = matchmaker(parents, recombinations) map procreate flatten

        val nextGen: Iterable[Individual[G]] = mutants ++ offspring

        debugger foreach { debug ⇒
          debug (
            generation,
            selectionIntensity(parents, nextGen),
            nextGen averageBy { _.fitness }
          )
        }

        assert(survivors == nextGen.size, "survivors = %d  mutants = %d  offspring = %d".format(
          survivors,
          mutants.size,
          offspring.size
        ))

        evolve(parents = nextGen, generation = generation + 1)
      }

    evolve(parents = ancestors(survivors), generation = 1)
  }
}
