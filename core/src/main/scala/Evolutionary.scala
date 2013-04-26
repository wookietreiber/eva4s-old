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

import language.higherKinds

/** Provides the basic functions of an evolutionary algorithm.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  *
  */
trait Evolutionary[G,P] {

  /** Returns the data structure representing the problem that needs to be solved.
    *
    * @note This data structure should be immutable or not be changed.
    */
  def problem: P

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome.
    *
    * @note The purpose of this method is the convenient creation of a new individual. It is just a
    * convenience wrapper around the case class to automatically inject the fitness according to
    * this evolutionary algorithm. Use it like the factory method of a case class.
    */
  final def Individual(genome: G): Individual[G] = new Individual(genome, fitness(genome))

}

object Evolutionary {
  def apply[G,P](p: P)(f: P ⇒ G ⇒ Double): Evolutionary[G,P] = new Evolutionary[G,P] {
    override val problem = p
    override def fitness(g: G) = f(problem)(g)
  }

  def simple[G,P](p: P)(f: G ⇒ Double): Evolutionary[G,P] = new Evolutionary[G,P] {
    override val problem = p
    override def fitness(g: G) = f(g)
  }
}


trait Full[G,P,M[_]] extends Evolutionary[G,P]
    with Creation[G,P]
    with Mutation[G,P]
    with PointMutation[G,P]
    with Recombination[G,P,M]
