/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2013  Christian Krause                                                         *
 *                                                                                               *
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


package org.eva4s

/** Provides the means to mutate genomes and individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  *
  * @define NewMutantInfo The purpose of this method is the convenient creation of a new mutant. Use
  * it like the factory method of a case class. This method performs the mutation on its own, it is
  * not needed to do this in advance.
  */
trait Mutation[G,P] {

  /** Returns a new genome by mutating the given one. */
  def mutate(genome: G): G

  /** Returns a new individual by mutating the given genome.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(genome: G): Individual[G] = Individual(mutate(genome))

  /** Returns a new individual by mutating the genome of the given individual.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(individual: Individual[G]): Individual[G] = Mutant(individual.genome)

  /** Returns the problem that needs to be solved. */
  def problem: P

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome. */
  def Individual(genome: G): Individual[G]

}

/** Standalone [[Mutation]] building block. */
trait Mutator[G,P] extends Mutation[G,P] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)

}

/** Factory for [[Mutator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define mutation mutation function
  */
object Mutator {

  /** Creates a new [[Mutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ G ⇒ G): Mutator[G,P] = new Mutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def mutate(genome: G): G = f(problem)(genome)
  }

  /** Creates a new [[Mutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation
    */
  def independent[G,P](e: Evolutionary[G,P])(f: G ⇒ G): Mutator[G,P] = new Mutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def mutate(genome: G): G = f(genome)
  }

}
