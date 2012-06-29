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


package ea
package foo

object Foo {

  def OnePointCrossover(parents: Pair[Vector[Boolean],Vector[Boolean]]): Iterable[Vector[Boolean]] = {
    require(parents._1.size == parents._2.size)

    val point = Random.nextInt(parents._1.size)

    val c1 = (parents._1 take point) ++ (parents._2 drop point)
    val c2 = (parents._2 take point) ++ (parents._1 drop point)

    var children = Iterable(c1, c2)

    assume(children forall { _.size == parents._1.size })

    children
  }

  def TwoPointCrossover(parents: Pair[Vector[Boolean],Vector[Boolean]]): Iterable[Vector[Boolean]] = {
    require(parents._1.size == parents._2.size)

    val p1 = Random.nextInt(parents._1.size)
    val p2 = Random.nextInt(parents._1.size)

    val (a,b) = p1.min(p2) → p1.max(p2)

    val c1 = (parents._1 take a) ++ (parents._2 take b drop a) ++ (parents._1 drop b)
    val c2 = (parents._2 take a) ++ (parents._1 take b drop a) ++ (parents._2 drop b)

    var children = Iterable(c1, c2)

    assume(children forall { _.size == parents._1.size })

    children
  }

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

import Foo._

class Foo(vars: Int, gl: Vector[Double], gu: Vector[Double])
         (override val problem: Vector[Double] ⇒ Double)
         (val recomb: Pair[Vector[Double],Vector[Double]] ⇒ Iterable[Vector[Double]])
  extends EvolutionaryAlgorithm[Vector[Double], Vector[Double] ⇒ Double] {

  override def ancestor: Vector[Double] = for {
    i ← Vector(1 to vars: _*)
  } yield gl(i) + (gu(i) - gl(i)) * Random.nextDouble

  override def fitness(g: Vector[Double]): Double = problem(g)

  override def mutate(g: Vector[Double]): Vector[Double] = g map { x ⇒
    (0.5 * Random.nextDouble + 0.75) * x
  }

  override def recombine(parents: Pair[Vector[Double],Vector[Double]]): Iterable[Vector[Double]] =
    recomb(parents)

}

class Bar(vars: Int, k: Int, gl: Vector[Double], gu: Vector[Double])(p: Vector[Double] ⇒ Double)
         (implicit recomb: Pair[Vector[Boolean],Vector[Boolean]] ⇒ Iterable[Vector[Boolean]] =
            TwoPointCrossover)
  extends EvolutionaryAlgorithm[Vector[Boolean], Vector[Boolean] ⇒ Double] {

  override val problem = (v: Vector[Boolean]) ⇒ {
    require(v.size == k * vars)
    p(decode(v))
  }

  override def ancestor: Vector[Boolean] = for {
    i ← Vector(1 to (k * vars): _*)
  } yield Random.nextBoolean

  override def fitness(g: Vector[Boolean]): Double = problem(g)

  def decode(v: Vector[Boolean]): Vector[Double] = for {
    i ← Vector(0 to (v.size / k) - 1: _*)
    gll = gl(i)
    gul = gu(i)
    a = v drop ((i-1) * k) take k
    s = 0 to (k-1) map { j ⇒ if (a(k-j-1)) math.pow(2, j) else 0 } sum
  } yield gl(i) + granularity(gl(i), gu(i)) * s

  def granularity(gl: Double, gu: Double) =
    (gu - gl) / (math.pow(2, k) - 1)

  override def mutate(g: Vector[Boolean]): Vector[Boolean] = {
    val i = Random.nextInt(g.size)
    g.updated(i, ! g(i))
  }

  override def recombine(parents: Pair[Vector[Boolean],Vector[Boolean]]): Iterable[Vector[Boolean]] =
    recomb(parents)

}
