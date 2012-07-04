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
  extends Matchmaking with Mutagens with Selection {

  /** Returns the data structure representing the problem that needs to be solved. */
  def problem: P

  /**
    *
    * @param generations amount of generations until the solution is chosen
    * @param survivors amount of survivors per generation as well as initial population / ancestors
    * @param matchmaker determines, which parents reproduce new children (strategy pattern, defaults
    * to [[ea.Matchmaking#RandomAcceptanceMatchmaker]])
    * @param mutagen chance of child to mutate as a function from current generation to a floating
    * point value between 0 and 1, defaults to [[ea.Mutagens#ExponentialMutagen]] with a start
    * probability of `0.8` and an end probability of `0.1`
    * @param selector determines, how the individuals for the next generation are chosen (strategy
    * pattern, defaults to [[ea.Selection#SurvivalOfTheFittest]])
    * @param debugger can be used to do some side effekt with the current generation and its optimal
    * fitness, e.g. print it: `debugger = printer`
    */
  def apply(generations: Int = 200,
            survivors: Int = 23)
           (implicit matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker(100, 0.7),
                     mutagen: Mutagen = ExponentialMutagen(0.8, 0.1)(generations),
                     selector: Selector[G] = SurvivalOfTheFittest,
                     debugger: Option[(Int,Double,Double) ⇒ Unit] = None)
            : Individual[G] = {
    @tailrec
    def evolve(parents: Iterable[Individual[G]], generation: Int): Individual[G] = if (generation == generations) {
      parents minBy { _.fitness }
    } else {
      val offspring = for {
        pair  ← matchmaker(parents)
        child ← recombine(mutagen(generation))(pair)
      } yield child

      val nextGen = selector(parents, offspring)

      // bail out if there is just one individual left
      if (nextGen.size == 1)
        return nextGen.head

      debugger foreach { debug ⇒
        debug(generation, selectionIntensity(parents ++ offspring, nextGen), nextGen averageBy { _.fitness })
      }

      evolve(parents = nextGen, generation = generation + 1)
    }

    evolve(parents = ancestors(survivors), generation = 1)
  }

  /** Returns a randomly generated genome. */
  def ancestor: G

  /** Returns the initial population. */
  final def ancestors(n: Int): Iterable[Individual[G]] = for {
    i ← 1 to n
    genome = ancestor
  } yield Individual(genome, fitness(genome))

  /** Returns a new genome by recombination. */
  def recombine(parents: Pair[G,G]): Iterable[G]

  /** Returns a new individual by recombination. */
  final def recombine(mutationProbability: Double)
                     (parents: Pair[Individual[G],Individual[G]])
                      : Iterable[Individual[G]] = for {
    genome ← recombine(parents._1.genome → parents._2.genome)
    mutant = if (Random.nextDouble < mutationProbability) mutate(genome) else genome
  } yield Individual(mutant, fitness(mutant))

  /** Returns a mutated genome. */
  def mutate(genome: G): G

  /** Returns the fitness of a genome. */
  def fitness(genome: G): Double

  /** Returns the selection intensity of the given generation. */
  def selectionIntensity(oldGen: Iterable[Individual[G]], newGen: Iterable[Individual[G]]): Double = {
    val fselbar = newGen averageBy { _.fitness }
    val fbar    = oldGen averageBy { _.fitness }

    val sigma = math.sqrt(
      (1.0 / (oldGen.size - 1)) * ((oldGen map { i ⇒ math.pow(fbar - i.fitness, 2) }).sum)
    )

    if (sigma != 0.0) (fselbar - fbar) / sigma else 0.0
  }

}
