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


package org.eva4s

/** Provides the basic functions of an evolutionary algorithm.
  *
  * @define HowManyInfo How many will be returned depends solely on the implementing evolutionary
  * algorithm.
  *
  * @define NewMutantInfo The purpose of this method is the convenient creation of a new mutant. Use
  * it like the factory method of a case class. This method performs the mutation on its own, it is
  * not needed to do this in advance.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  */
trait Evolutionary[G,P] {

  /** Returns the data structure representing the problem that needs to be solved.
    *
    * @note This data structure should be immutable or not be changed.
    */
  val problem: P

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome.
    *
    * @note The purpose of this method is the convenient creation of a new individual. It is just a
    * convenience wrapper around [[org.eva4s.package.Individual]] to automatically inject the
    * fitness according to this evolutionary algorithm. Use it like the factory method of a case
    * class.
    */
  final def Individual(genome: G): Individual[G] =
    new Individual(genome, fitness(genome))

  // -----------------------------------------------------------------------------------------------
  // ancestors / initial population
  // -----------------------------------------------------------------------------------------------

  /** Returns a generated genome.
    *
    * @note Ancestors are used for the initial population.
    */
  def ancestor: G

  /** Returns the initial population.
    *
    * @param n the amount of ancestors to create
    */
  final def ancestors(n: Int): Iterable[Individual[G]] =
    Vector.fill(n)(Individual(ancestor))

  // -----------------------------------------------------------------------------------------------
  // recombination
  // -----------------------------------------------------------------------------------------------

  /** Returns new genomes by recombining the parents.
    *
    * @note $HowManyInfo
    */
  def recombine(p1: G, p2: G): Iterable[G]

  /** Returns new genomes by recombining the parents.
    *
    * @note $HowManyInfo
    */
  final def recombine(parents: Pair[Individual[G],Individual[G]]): Iterable[G] =
    recombine(parents._1.genome, parents._2.genome)

  /** Returns new individuals by recombining the parents.
    *
    * @note $HowManyInfo
    */
  final def procreate(parents: Pair[Individual[G],Individual[G]]): Iterable[Individual[G]] = for {
    genome ← recombine(parents)
  } yield Individual(genome)

  // -----------------------------------------------------------------------------------------------
  // mutation
  // -----------------------------------------------------------------------------------------------

  /** Returns a new genome by mutating the given one. */
  def mutate(genome: G): G

  /** Returns a new individual by mutating the given genome.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(genome: G): Individual[G] =
    Individual(mutate(genome))

  /** Returns a new individual by mutating the genome of the given individual.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(individual: Individual[G]): Individual[G] =
    Mutant(individual.genome)

}
