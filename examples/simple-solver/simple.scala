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
package simple

import language.higherKinds

object Main extends App with Evolvers {
  type Genome = Double
  type Problem = Double ⇒ Double

  implicit val evolutionary = Evolutionary((x: Double) ⇒ x*x + 4) {
    (problem: Problem) ⇒ (genome: Double) ⇒ problem(genome)
  }

  implicit val creator = Creator.independent(evolutionary) {
    (Random.nextInt(10000) - 5000).toDouble
  }

  implicit val mutator = Mutator.independent(evolutionary) {
    (genome: Double) ⇒ genome + Random.nextInt(9) - 4
  }

  implicit val pmutator = PointMutator.independent(evolutionary) {
    (genome: Double) ⇒ genome + Random.nextInt(3) - 1
  }

  implicit val recombinator = OnlyChildRecombinator.independent(evolutionary) {
    (g1: Double, g2: Double) ⇒ (g1 + g2) / 2
  }

  println(SingleEvolver())
}

object MainFull extends App with Evolvers {
  type G = Double
  type P = Double ⇒ Double

  class Solver(val problem: P) extends Full[G,P,scalaz.Id.Id] {
    def fitness(genome: Double) = problem(genome)
    def ancestor = (Random.nextInt(10000) - 5000).toDouble
    def mutate(genome: Double) = genome + Random.nextInt(9) - 4
    def pmutate(genome: Double) = genome + Random.nextInt(3) - 1
    def recombine(g1: Double, g2: Double) = (g1 + g2) / 2
  }

  val solver = new Solver((x: Double) ⇒ x*x + 4)

  println(SingleEvolver.full(solver)())
}
