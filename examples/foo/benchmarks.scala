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

import math._

object Benchmark {

  def foonchmark(n: Int, gmin: Double, gmax: Double)
         (f: Vector[Double] ⇒ Double)
         (generations: Int = 200, individuals: Int = 100)
         (cross: ((Vector[Double], Vector[Double])) ⇒ Iterable[Vector[Double]])
         : (Int,Vector[Double],Double) = {
    val mins = Vector.fill(n)(gmin)
    val maxs = Vector.fill(n)(gmax)
    val foo = new Foo(n, mins, maxs)(f)(cross)
    val individual = SplitEvolver(foo)(generations, individuals)(debugger = printer)
    (n,individual.genome,individual.fitness)
  }

  def barnchmark(n: Int, k: Int, gmin: Double, gmax: Double)
         (f: Vector[Double] ⇒ Double)
         (generations: Int = 200, individuals: Int = 100)
         (cross: ((Vector[Boolean], Vector[Boolean])) ⇒ Iterable[Vector[Boolean]])
         : (Int,Vector[Double],Double) = {
    val mins = Vector.fill(n)(gmin)
    val maxs = Vector.fill(n)(gmax)
    val bar = new Bar(n, k, mins, maxs)(f)(cross)
    val individual = SplitEvolver(bar)(generations, individuals)()
    (n,bar.decode(individual.genome),individual.fitness)
  }

}
