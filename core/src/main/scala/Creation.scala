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

trait Creation[G,P] {

  /** Returns a randomly generated genome.
    *
    * @note Ancestors are used for the initial population.
    */
  def ancestor: G

  final def Ancestor: Individual[G] = Individual(ancestor)

  def problem: P
  def fitness(genome: G): Double
  def Individual(genome: G): Individual[G]
}

trait Creator[G,P] extends Creation[G,P] {
  def evolutionary: Evolutionary[G,P]
  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)
}

object Creator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    def ancestor: G = f(problem)
  }

  def unbiased[G, P](ep: Evolutionary[G,P])(f: ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    def ancestor: G = f
  }
}
