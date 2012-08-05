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

  def OnePointCrossover(p1: Vector[Boolean], p2: Vector[Boolean]): Iterable[Vector[Boolean]] = {
    require(p1.size == p2.size)

    val point = Random.nextInt(p1.size)

    val c1 = (p1 take point) ++ (p2 drop point)
    val c2 = (p2 take point) ++ (p1 drop point)

    var children = Iterable(c1, c2)

    assume(children forall { _.size == p1.size })

    children
  }

  def TwoPointCrossover(p1: Vector[Boolean], p2: Vector[Boolean]): Iterable[Vector[Boolean]] = {
    require(p1.size == p2.size)

    val px1 = Random.nextInt(p1.size)
    val px2 = Random.nextInt(p1.size)

    val (a,b) = px1.min(px2) → px1.max(px2)

    val c1 = (p1 take a) ++ (p2 take b drop a) ++ (p1 drop b)
    val c2 = (p2 take a) ++ (p1 take b drop a) ++ (p2 drop b)

    var children = Iterable(c1, c2)

    assume(children forall { _.size == p1.size })

    children
  }

}

class BinarySolver(val vars: Int, val k: Int, val lower: Vector[Double], val upper: Vector[Double])
                  (p: Vector[Double] ⇒ Double)
                  (implicit recomb: (Vector[Boolean],Vector[Boolean]) ⇒ Iterable[Vector[Boolean]] =
                     BinarySolver.TwoPointCrossover)
  extends EvolutionarySolver[Boolean] {

  override val problem = (v: Vector[Boolean]) ⇒ {
    require(v.size == k * vars)
    p(decode(v))
  }

  override def ancestor: Vector[Boolean] = for {
    i ← Vector(1 to (k * vars): _*)
  } yield Random.nextBoolean

  def decode(v: Vector[Boolean]): Vector[Double] = for {
    i ← Vector(0 to (v.size / k) - 1: _*)
    a = v drop ((i-1) * k) take k
    s = 0 to (k-1) map { j ⇒ if (a(k-j-1)) math.pow(2, j) else 0 } sum
  } yield lower(i) + granularity(lower(i), upper(i)) * s

  def granularity(lower: Double, upper: Double) =
    (upper - lower) / (math.pow(2, k) - 1)

  override def mutate(g: Vector[Boolean]): Vector[Boolean] = {
    val i = Random.nextInt(g.size)
    g.updated(i, ! g(i))
  }

  override def recombine(p1: Vector[Boolean], p2: Vector[Boolean]): Iterable[Vector[Boolean]] =
    recomb(p1,p2)

}
