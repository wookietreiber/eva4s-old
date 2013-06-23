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

/** Provides a mechanism for point mutation. It differs from [[Mutation]] in the aspect that it
  * mutates genomes only slightly.
  *
  * == Uses Cases ==
  *
  * - [[Evolvers]] may use point mutation just before recombination to provide more diversity
  *
  * - point mutation may also be used to model external events / natural mutagens like gamma rays
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  *
  * @define NewMutantInfo The purpose of this method is the convenient creation of a new mutant. Use
  * it like the factory method of a case class. This method performs the mutation on its own, it is
  * not needed to do this in advance.
  */
trait PointMutation[G,P] {

  /** Returns a new genome by slightly mutating the given genome. */
  def pmutate(genome: G): G

  /** Returns a new individual by point mutating the given genome.
    *
    * @note $NewMutantInfo
    */
  final def PointMutant(genome: G): Individual[G] = Individual(pmutate(genome))

  /** Returns a new individual by point mutating the genome of the given individual.
    *
    * @note $NewMutantInfo
    */
  final def PointMutant(individual: Individual[G]): Individual[G] = PointMutant(individual.genome)

  /** Returns the problem that needs to be solved. */
  def problem: P

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome. */
  def Individual(genome: G): Individual[G]

}

/** Standalone [[PointMutation]] building block. */
trait PointMutator[G,P] extends PointMutation[G,P] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)

}

/** Factory for [[PointMutator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define mutation point mutation function
  */
object PointMutator {

  /** Creates a new [[PointMutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def pmutate(genome: G): G = f(problem)(genome)
  }

  /** Creates a new [[PointMutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation
    */
  def independent[G,P](e: Evolutionary[G,P])(f: G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def pmutate(genome: G): G = f(genome)
  }

}
