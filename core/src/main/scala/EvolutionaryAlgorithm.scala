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


package ea

import scala.annotation.tailrec

/**
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  */
trait EvolutionaryAlgorithm[G,P]
  extends Matchmaking with Selection {

  /** Returns the data structure representing the problem that needs to be solved. */
  def problem: P

  /**
    *
    * @param generations amount of generations until the solution is chosen
    * @param survivors amount of survivors per generation as well as initial population / ancestors
    * @param mutationProbability chance of child to mutate
    * @param matchmaker determines, which parents reproduce new children (strategy pattern, defaults
    * to [[ea.Matchmaking#RandomAcceptanceMatchmaker]])
    * @param selector determines, how the individuals for the next generation are chosen (strategy
    * pattern, defaults to [[ea.Selection#SurvivalOfTheFittest]])
    * @param debugger can be used to do some side effekt with the current generation and its optimal
    * fitness, e.g. print it: `debugger = Some((g,f) ⇒ printf("gen: %5d     fit: %f\n", g, f))`
    */
  def apply(generations: Int = 200,
            survivors: Int = 23,
            mutationProbability: Double = 0.3)
           (implicit matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker(100, 0.7),
                     selector: Selector[G] = SurvivalOfTheFittest,
                     debugger: Option[(Int,Double) ⇒ Unit] = None)
            : Individual[G] = {

    @tailrec
    def evolve(parents: Iterable[Individual[G]], generation: Int): Individual[G] = if (generation == generations) {
      parents minBy { _.fitness }
    } else {
      val offspring = for {
        pair   ← matchmaker(parents)
        child  = recombine(mutationProbability)(pair)
      } yield child

      val nextGen = selector(parents, offspring)

      // bail out if there is just one individual left
      if (nextGen.size == 1)
        return nextGen.head

      debugger foreach { debug ⇒
        debug(generation, nextGen minBy { _.fitness } fitness)
      }

      evolve(parents = nextGen, generation = generation + 1)
    }

    evolve(parents = ancestors(survivors), generation = 1)
  }

  /** Returns a randomly generated ancestor solution. */
  def ancestor: G

  /** Returns the initial population. */
  final def ancestors(n: Int): Iterable[Individual[G]] = for {
    i ← 1 to n
    genome = ancestor
  } yield Individual(genome, fitness(genome))

  /** Returns a new genome by recombination. */
  def recombine(parents: Pair[G,G]): G

  /** Returns a new individual by recombination. */
  final def recombine(mutationProbability: Double)
                     (parents: Pair[Individual[G],Individual[G]])
                      : Individual[G] = {
    var genome = recombine(parents._1.genome → parents._2.genome)

    if (Random.nextDouble < mutationProbability)
      genome = mutate(genome)

    Individual(genome, fitness(genome))
  }

  /** Returns a mutated genome. */
  def mutate(genome: G): G

  /** Returns the fitness of a genome. */
  def fitness(genome: G): Double

}
