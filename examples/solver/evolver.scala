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
package solver

import language.postfixOps

import scala.annotation.tailrec
import scalay.collection._

import Evolvers._

object SplitEvolver extends Evolver {
  def apply[G,P](ea: Evolutionary[G,P])
                (generations: Int = 500,
                 individuals: Int = 100)
                (implicit matchmaker: Matchmaker[G] = RankBasedMatchmaker[G] _,
                          mutagen: Mutagen = ExponentialMutagen(generations),
                          debugger: Option[(Int,Double) ⇒ Unit] = None)
                 : Individual[G] = {
    import ea._

    @tailrec
    def evolve(parents: Seq[Individual[G]], generation: Int): Individual[G] =
      if (generation == generations) {
        parents minBy { _.fitness }
      } else {
        val recombinations = (individuals * (1.0 - mutagen(generation)) / 2.0).round.toInt
        val mutations      = individuals - (2 * recombinations)

        val mutants   = parents choose mutations map Mutant
        val offspring = matchmaker(parents, recombinations) map procreate flatten

        val nextGen = mutants ++ offspring

        debugger foreach { debug ⇒ debug(generation, nextGen geometricMeanBy { _.fitness }) }

        evolve(parents = nextGen, generation = generation + 1)
      }

    evolve(parents = ancestors(individuals), generation = 1)
  }
}
