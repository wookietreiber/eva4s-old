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
  * @tparam A input type, represents the problem data structure
  * @tparam Individual result / output type, represents solutions of the problem - the individuals
  */
trait EvolutionaryAlgorithm[A,Individual] {

  /** Returns the data structure representing the problem that needs to be solved. */
  def problem: A

  /**
    *
    * @param generations amount of generations until the solution is chosen
    * @param survivors amount of survivors per generation as well as initial population / ancestors
    * @param recombinations possible recombinations / children per generation
    * @param recombinationProbability chance of recombination / child per generation
    * @param mutationProbability chance of child to mutate
    */
  def apply(
      generations: Int = 2000,
      survivors: Int = 10,
      recombinations: Int = 40,
      recombinationProbability: Double = 0.3,
      mutationProbability: Double = 0.3
    ): Individual = {

    @tailrec
    def evolve(oldGen: Iterable[Individual], generations: Int): Individual = if (generations == 0) {
      oldGen minBy fitness
    } else {
      val children = for {
        i ← 1 to recombinations if Random.nextDouble < recombinationProbability
        parents = choose(2)(oldGen)
        child   = recombine(parents)
      } yield if (Random.nextDouble < mutationProbability)
        mutate(child)
      else
        child

      val nextGen = (oldGen ++ children).toSeq sortBy fitness take survivors

      evolve(nextGen, generations - 1)
    }

    evolve(ancestors(survivors), generations)
  }

  /** Selects the fittest `n` individuals. */
  def select(n: Int)(individuals: Iterable[Individual]): Iterable[Individual] =
    individuals.toSeq sortBy fitness take n

  /** Returns a randomly generated ancestor solution. */
  def ancestor: Individual

  /** Returns the initial population. */
  def ancestors(n: Int): Iterable[Individual] =
    for (i ← 1 to n) yield ancestor

  /** Returns a new individual by recombining the given individuals. */
  def recombine(individual: Iterable[Individual]): Individual

  /** Returns a new child. */
  final def procreate(parents: Iterable[Individual]): Individual =
    recombine(parents)

  /** Returns a mutated individual. */
  def mutate(individual: Individual): Individual

  /** Returns the fitness of an individual. */
  def fitness(individual: Individual): Double

}
