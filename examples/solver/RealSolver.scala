/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2012  Nils Foken, Christian Krause                                             *
 *                                                                                               *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                                          *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>                                    *
 *                    <kizkizzbangbang@googlemail.com>                                           *
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
package solver

object RealSolver {

  def IntermediateCrossover(p1: Vector[Double], p2: Vector[Double]): Iterable[Vector[Double]] = {
    require(p1.size == p2.size)

    def sample(a: Double) = - a + (1 + 2 * a) * Random.nextDouble

    val size = p1.size

    var children = for {
      i ← 1 to 2
      ss = for { i ← 1 to size } yield sample(0.25)
      a = p1 zip ss map { case (a,b) ⇒ a*b }
      b = p2 zip ss map { case (a,b) ⇒ a*(1-b) }
    } yield a zip b map { case (a,b) ⇒ a+b }

    assume(children forall { _.size == p1.size })

    children
  }

  def LineCrossover(p1: Vector[Double], p2: Vector[Double]): Iterable[Vector[Double]] = {
    require(p1.size == p2.size)

    def sample(a: Double) = - a + (1 + 2 * a) * Random.nextDouble

    val size = p1.size

    var children = for {
      i ← 1 to 2
      ss = for { i ← 1 to size ; s = sample(0.25) } yield s
      a = p1 zip ss map { case (a,b) ⇒ a*b }
      b = p2 zip ss map { case (a,b) ⇒ a*(1-b) }
    } yield a zip b map { case (a,b) ⇒ a+b }

    assume(children forall { _.size == p1.size })

    children
  }

  def ArithmeticCrossover(p1: Vector[Double], p2: Vector[Double]): Iterable[Vector[Double]] = {
    require(p1.size == p2.size)

    val c1 = p1 zip p2 map { case (a,b) ⇒ (a+b) / 2 }
    val c2 = p1 zip p2 map { case (a,b) ⇒ math.sqrt(a*b) }

    var children = Iterable(c1, c2)

    assume(children forall { _.size == p1.size })

    children
  }

}

class RealSolver(val vars: Int, val lower: Vector[Double], val upper: Vector[Double])
         (override val problem: Equation)
         (implicit recomb: (Vector[Double],Vector[Double]) ⇒ Iterable[Vector[Double]] =
            RealSolver.IntermediateCrossover)
  extends EvolutionarySolver[Double] {

  def this(vars: Int, problem: BoundedEquation,
           recomb: (Vector[Double],Vector[Double]) ⇒ Iterable[Vector[Double]]) = this(
    vars,
    Vector.fill(vars)(problem.lower),
    Vector.fill(vars)(problem.upper)
  )(problem)(recomb)

  def this(vars: Int, problem: BoundedEquation) = this(
    vars,
    Vector.fill(vars)(problem.lower),
    Vector.fill(vars)(problem.upper)
  )(problem)

  override def ancestor: Vector[Double] = boundedAncestor

  override def mutate(g: Vector[Double]): Vector[Double] = g map { x ⇒
    (0.5 * Random.nextDouble + 0.75) * x
  }

  override def recombine(p1: Vector[Double], p2: Vector[Double]) =
    recomb(p1,p2)

}
