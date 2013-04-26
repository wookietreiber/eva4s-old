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

/** Provides a mechanism for point mutation.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  */
trait PointMutation[G,P] {

  /** Returns a new genome by slightly mutating the given genome. */
  def pmutate(genome: G): G

  final def PointMutant(genome: G): Individual[G] = Individual(pmutate(genome))

  def problem: P
  def fitness(genome: G): Double
  def Individual(genome: G): Individual[G]
}

trait PointMutator[G,P] extends PointMutation[G,P] {
  def evolutionary: Evolutionary[G,P]
  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)
}

object PointMutator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def pmutate(genome: G): G = f(problem)(genome)
  }

  def unbiased[G,P](ep: Evolutionary[G,P])(f: G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def pmutate(genome: G): G = f(genome)
  }
}
