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
package solver

import math._

object Equation {

  def ackley(xs: Vector[Double]): Double = {
    val a = 20

    val b = - 0.2 * sqrt((xs map { x ⇒ x*x } sum) / xs.size)
    val c = 20 * exp(b)

    val d = exp(((xs map { x ⇒ cos(2 * Pi * x) } sum) / xs.size))

    a + E - c - d
  }

  def griewank(xs: Vector[Double]): Double = {
    val a = 1
    val b = xs map { x ⇒
      pow(x,2) / (400 * xs.size)
    } sum
    val c = xs.zipWithIndex map {
      case (x,i) ⇒ cos(x / sqrt(i+1))
    } product

    a + b - c
  }

  def cfunc(xs: Vector[Double]): Double = {
    val as = for {
      i ←  1    to (xs.size - 1)
      j ← (i+1) to (xs.size    )
      a = abs(xs(j-1) - xs(i-1))
      b = j - i
    } yield (a / b)

    2 * as.sum
  }

  def ns(xs: Vector[Double])/*: Double*/ = {
    val n = xs.size

    val alphas = xs.zipWithIndex map {
      case (x,i) if i + 1 == n ⇒ xs.product - 1
      case (x,i)               ⇒ xs(i) + xs.sum - n - 1
    }

    //  n * pow(alpha,n) - (n+1) * pow(alpha, n-1) + 1 = 0
  }

}
