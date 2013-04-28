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

/** Provides the creation of randomly generated individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  */
trait Creation[G,P] {

  /** Returns a randomly generated genome. */
  def ancestor: G

  /** Returns a randomly generated individual. */
  final def Ancestor: Individual[G] = Individual(ancestor)

  /** Returns the problem that needs to be solved. */
  def problem: P

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome. */
  def Individual(genome: G): Individual[G]

}

/** Standalone [[Creation]] building block. */
trait Creator[G,P] extends Creation[G,P] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)

}

/** Factory for [[Creator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define creation creation function for generating a random genome
  */
object Creator {

  /** Creates a new [[Creator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $creation, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    def ancestor: G = f(problem)
  }

  /** Creates a new [[Creator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $creation
    */
  def independent[G, P](e: Evolutionary[G,P])(f: ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    def ancestor: G = f
  }

}
