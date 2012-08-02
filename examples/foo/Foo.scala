/* **************************************************************************
 *                                                                          *
 *  Copyright (C)  2012  Nils Foken, Christian Krause                       *
 *                                                                          *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                     *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>               *
 *                                                                          *
 ****************************************************************************
 *                                                                          *
 *  This file is part of 'scalevalgo'.                                      *
 *                                                                          *
 *  This project is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by    *
 *  the Free Software Foundation, either version 3 of the License, or       *
 *  any later version.                                                      *
 *                                                                          *
 *  This project is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 *  GNU General Public License for more details.                            *
 *                                                                          *
 *  You should have received a copy of the GNU General Public License       *
 *  along with this project. If not, see <http://www.gnu.org/licenses/>.    *
 *                                                                          *
 ****************************************************************************/


package org.eva4s
package foo

object Foo {

  def IntermediateCrossover(parents: Pair[Vector[Double],Vector[Double]]): Iterable[Vector[Double]] = {
    require(parents._1.size == parents._2.size)

    def sample(a: Double) = - a + (1 + 2 * a) * Random.nextDouble

    val size = parents._1.size

    var children = for {
      i ← 1 to 2
      ss = for { i ← 1 to size } yield sample(0.25)
      a = parents._1 zip ss map { case (a,b) ⇒ a*b }
      b = parents._2 zip ss map { case (a,b) ⇒ a*(1-b) }
    } yield a zip b map { case (a,b) ⇒ a+b }

    assume(children forall { _.size == parents._1.size })

    children
  }

  def LineCrossover(parents: Pair[Vector[Double],Vector[Double]]): Iterable[Vector[Double]] = {
    require(parents._1.size == parents._2.size)

    def sample(a: Double) = - a + (1 + 2 * a) * Random.nextDouble

    val size = parents._1.size

    var children = for {
      i ← 1 to 2
      ss = for { i ← 1 to size ; s = sample(0.25) } yield s
      a = parents._1 zip ss map { case (a,b) ⇒ a*b }
      b = parents._2 zip ss map { case (a,b) ⇒ a*(1-b) }
    } yield a zip b map { case (a,b) ⇒ a+b }

    assume(children forall { _.size == parents._1.size })

    children
  }

  def ArithmeticCrossover(parents: Pair[Vector[Double],Vector[Double]]): Iterable[Vector[Double]] = {
    require(parents._1.size == parents._2.size)

    val c1 = parents._1 zip parents._2 map { case (a,b) ⇒ (a+b) / 2 }
    val c2 = parents._1 zip parents._2 map { case (a,b) ⇒ math.sqrt(a*b) }

    var children = Iterable(c1, c2)

    assume(children forall { _.size == parents._1.size })

    children
  }

}

class Foo(vars: Int, gl: Vector[Double], gu: Vector[Double])
         (override val problem: Vector[Double] ⇒ Double)
         (implicit val recomb: Pair[Vector[Double],Vector[Double]] ⇒ Iterable[Vector[Double]] =
            Foo.IntermediateCrossover)
  extends Evolutionary[Vector[Double], Vector[Double] ⇒ Double] {

  override def ancestor: Vector[Double] = for {
    i ← Vector(1 to vars: _*)
  } yield gl(i-1) + (gu(i-1) - gl(i-1)) * Random.nextDouble

  override def fitness(g: Vector[Double]): Double = problem(g)

  override def mutate(g: Vector[Double]): Vector[Double] = g map { x ⇒
    (0.5 * Random.nextDouble + 0.75) * x
  }

  override def recombine(p1: Vector[Double], p2: Vector[Double]): Iterable[Vector[Double]] =
    recomb(p1,p2)

}
