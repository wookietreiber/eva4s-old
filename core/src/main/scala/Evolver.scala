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

import scala.annotation.tailrec
import scalay.collection._

trait Evolver {

  implicit val DoubleIntegral: Integral[Double] = scala.math.Numeric.DoubleAsIfIntegral

  /** Returns the selection intensity of the given generation. */
  def selectionIntensity(oldGen: Seq[Individual[_]],
                         newGen: Seq[Individual[_]]): Double = {

    val fselbar = newGen averageBy { _.fitness }
    val fbar    = oldGen averageBy { _.fitness }

    val sigma = math.sqrt(
      (1.0 / (oldGen.size - 1)) * ((oldGen map { i ⇒ math.pow(fbar - i.fitness, 2) }).sum)
    )

    if (sigma != 0.0) (fselbar - fbar) / sigma else 0.0
  }

}

/** $EvolverInfo */
object Evolvers extends Evolvers

/** $EvolverInfo
  *
  * @define EvolverInfo To be done.
  */
trait Evolvers extends Matchmaking with Mutagens with Selection {

  object DefaultEvolver extends Evolver {

    /**
      *
      * @param generations amount of generations until the solution is chosen
      * @param survivors amount of survivors per generation as well as initial population / ancestors
      * @param pairs amount of pairs generated per generation
      * @param matchmaker determines, which parents reproduce new children (strategy pattern, defaults
      * to [[org.eva4s.Matchmaking#RandomAcceptanceMatchmaker]])
      * @param mutagen chance of child to mutate as a function from current generation to a floating
      * point value between 0 and 1, defaults to [[org.eva4s.Mutagens#ExponentialMutagen]] with a
      * start probability of `0.8` and an end probability of `0.1`
      * @param selector determines, how the individuals for the next generation are chosen (strategy
      * pattern, defaults to [[org.eva4s.Selection#SurvivalOfTheFittest]])
      * @param debugger can be used to do some side effekt with the current generation and its optimal
      * fitness, e.g. print it: `debugger = printer`
      */
    def apply[G,P](ea: Evolutionary[G,P])
                  (generations: Int = 200,
                   survivors: Int = 23,
                   pairs: Int = 100)
                  (implicit matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
                            mutagen: Mutagen = ExponentialMutagen(generations),
                            selector: Selector[G] = SurvivalOfTheFittest[G] _,
                            debugger: Option[(Int,Double,Double) ⇒ Unit] = None)
                   : Individual[G] = {
      import ea._

      @tailrec
      def evolve(parents: Seq[Individual[G]], generation: Int): Individual[G] =
      if (generation == generations) {
        parents minBy { _.fitness }
      } else {
        val offspring = for {
          pair   ← matchmaker(parents, pairs)
          genome ← recombine(pair)
        } yield if (Random.nextDouble < mutagen(generation))
          Mutant(genome)
        else
          Individual(genome)

        val nextGen = selector(parents, offspring)

        // bail out if there is just one individual left
        if (nextGen.size == 1)
          return nextGen.head

        debugger foreach { debug ⇒
          debug (
            generation,
            selectionIntensity(parents ++ offspring, nextGen),
            nextGen averageBy { _.fitness }
          )
        }

        evolve(parents = nextGen, generation = generation + 1)
      }

      evolve(parents = ancestors(survivors), generation = 1)
    }
  }

}
