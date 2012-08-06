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

object BinarySolver {

  def decode(xs: Vector[Boolean],
             lower: Vector[Double],
             upper: Vector[Double],
             bits: Int): Vector[Double] = Vector.tabulate(xs.size / bits) { i ⇒
    // take current encoded booleans
    val x = xs drop ((i-1) * bits) take bits

    val s = Vector.tabulate(bits) { i ⇒
      if (x(bits-i-1)) math.pow(2, i) else 0
    } sum

    lower(i) + granularity(lower(i), upper(i), bits) * s
  }

  def granularity(lower: Double, upper: Double, bits: Int): Double =
    (upper - lower) / (math.pow(2, bits) - 1)

  def OnePointCrossover(p1: Vector[Boolean], p2: Vector[Boolean]): Iterable[Vector[Boolean]] = {
    val point = Random.nextInt(p1.size)

    val c1 = (p1 take point) ++ (p2 drop point)
    val c2 = (p2 take point) ++ (p1 drop point)

    Vector(c1, c2)
  }

  def TwoPointCrossover(p1: Vector[Boolean], p2: Vector[Boolean]): Iterable[Vector[Boolean]] = {
    val point1 = Random.nextInt(p1.size)
    val point2 = Random.nextInt(p1.size)

    val a = point1 min point2
    val b = point1 max point2

    val c1 = (p1 take a) ++ (p2 take b drop a) ++ (p1 drop b)
    val c2 = (p2 take a) ++ (p1 take b drop a) ++ (p2 drop b)

    Vector(c1, c2)
  }

}

class BinarySolver(val vars: Int, val bits: Int, val lower: Vector[Double], val upper: Vector[Double])
                  (p: Equation)
                  (implicit recomb: (Vector[Boolean],Vector[Boolean]) ⇒ Iterable[Vector[Boolean]] =
                     BinarySolver.TwoPointCrossover)
  extends EvolutionarySolver[Boolean] {

  def this(vars: Int, problem: BoundedEquation) = this (
    vars,
    problem.bits,
    Vector.fill(vars)(problem.lower),
    Vector.fill(vars)(problem.upper)
  )(problem)

  override val problem = (xs: Vector[Boolean]) ⇒ {
    val ys = BinarySolver.decode(xs, lower, upper, bits)
    p(ys)
  }

  override def ancestor: Vector[Boolean] = Vector.fill(bits * vars) {
    Random.nextBoolean
  }

  override def mutate(g: Vector[Boolean]): Vector[Boolean] = {
    val i = Random.nextInt(g.size)
    g.updated(i, ! g(i))
  }

  override def recombine(p1: Vector[Boolean], p2: Vector[Boolean]): Iterable[Vector[Boolean]] =
    recomb(p1,p2)

}
