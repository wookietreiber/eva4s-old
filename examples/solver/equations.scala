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

import math._

object Equation {

  /** Returns the [[http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO_files/Page295.htm Ackley function]]. */
  val ackley: Equation = new Function[Vector[Double],Double] {
    def apply(xs: Vector[Double]) = {
      val a = 20

      val b = - 0.2 * sqrt((xs map { x ⇒ x*x } sum) / xs.size)
      val c = 20 * exp(b)

      val d = exp(((xs map { x ⇒ cos(2 * Pi * x) } sum) / xs.size))

      a + E - c - d
    }

    override def toString = "Ackley Function"
  }

  /** Returns the [[http://mathworld.wolfram.com/GriewankFunction.html Griewank function]]. */
  val griewank: Equation = new Function[Vector[Double],Double] {
    def apply(xs: Vector[Double]) = {
      val a = xs.view map { pow(_,2) } sum
      val b = xs.view.zipWithIndex map {
        case (x,i) ⇒ cos(x / sqrt(i+1))
      } product

      1 + a/4000 - b
    }

    override def toString = "Griewank Function"
  }

  val cfunc: Equation = (xs: Vector[Double]) ⇒ {
    val as = for {
      i ←  1    to (xs.size - 1)
      j ← (i+1) to (xs.size    )
      a = abs(xs(j-1) - xs(i-1))
      b = j - i
    } yield (a / b)

    2 * as.sum
  }

  val ns: Equation = (xs: Vector[Double]) ⇒ {
    val n = xs.size

    val alphas = xs.zipWithIndex map {
      case (x,i) if i + 1 == n ⇒ xs.product - 1
      case (x,i)               ⇒ xs(i) + xs.sum - n - 1
    }

    //  n * pow(alpha,n) - (n+1) * pow(alpha, n-1) + 1 = 0
    0.0
  }

}
