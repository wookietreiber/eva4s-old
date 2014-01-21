package eva4s
package util

import language.higherKinds

import scala.collection.GenTraversable

trait Statistics[M[_]] {
  def arithmeticMean[A:Integral](xs: M[A]): A
  def arithmeticMeanBy[A,B:Integral](xs: M[A])(f: A => B): B

  def geometricMean[A:Numeric](xs: M[A]): Double
  def geometricMeanBy[A,B:Numeric](xs: M[A])(f: A => B): Double

  def harmonicMean[A:Integral](xs: M[A]): A
  def harmonicMeanBy[A,B:Integral](xs: M[A])(f: A => B): B

  def quadraticMean[A:Numeric](xs: M[A]): Double
  def quadraticMeanBy[A,B:Numeric](xs: M[A])(f: A => B): Double

  def median[A](xs: M[A])(implicit sorter: Sorter[M], int: Integral[A]): A
  def medianBy[A,B](xs: M[A])(f: A => B)(implicit sorter: Sorter[M], int: Integral[B]): B
}

trait StatisticsLow {
  implicit def GenTraversableStatistics[CC[X] <: GenTraversable[X]]: Statistics[CC] = new Statistics[CC] {
    def arithmeticMean[A](xs: CC[A])(implicit int: Integral[A]): A = {
      import int._
      xs.aggregate(zero)(_ + _, _ + _) / fromInt(xs.size)
    }

    def arithmeticMeanBy[A,B](xs: CC[A])(f: A => B)(implicit int: Integral[B]): B = {
      import int._
      xs.aggregate(zero)(_ + f(_), _ + _) / fromInt(xs.size)
    }

    def geometricMean[A](xs: CC[A])(implicit num: Numeric[A]): Double = {
      import num._
      math.pow(xs.product.toDouble, 1.0 / xs.size)
    }

    def geometricMeanBy[A,B](xs: CC[A])(f: A => B)(implicit num: Numeric[B]): Double = {
      import num._
      val acc = xs.aggregate(one)(_ * f(_), _ * _).toDouble
      math.pow(acc, 1.0 / xs.size)
    }

    def harmonicMean[A](xs: CC[A])(implicit int: Integral[A]): A = {
      import int._
      val acc = xs.aggregate(zero)(_ + one / _, _ + _)
      fromInt(xs.size) / acc
    }

    def harmonicMeanBy[A,B](xs: CC[A])(f: A => B)(implicit int: Integral[B]): B = {
      import int._
      val acc = xs.aggregate(zero)(_ + one / f(_), _ + _)
      fromInt(xs.size) / acc
    }

    def quadraticMean[A](xs: CC[A])(implicit num: Numeric[A]): Double = {
      import num._
      val acc = xs.aggregate(zero)( (a,b) => a + b*b, _ + _).toDouble
      math.sqrt(acc / xs.size)
    }

    def quadraticMeanBy[A,B](xs: CC[A])(f: A => B)(implicit num: Numeric[B]): Double = {
      import num._
      val acc = xs.aggregate(zero)((acc,a) => { val b = f(a) ; b*b }, _ + _).toDouble
      math.sqrt(acc / xs.size)
    }

    def median[A](xs: CC[A])(implicit sorter: Sorter[CC], int: Integral[A]): A = {
      import int._

      val sorted = sorter.sort(xs).toIndexedSeq
      val n = sorted.size

      if (n % 2 == 0)
        (sorted(n/2) + sorted(n/2 + 1)) / (fromInt(2))
      else
        sorted((n+1)/2)
    }

    def medianBy[A,B](xs: CC[A])(f: A => B)(implicit sorter: Sorter[CC], int: Integral[B]): B = {
      import int._

      val sorted = sorter.sortBy(xs)(f).toIndexedSeq
      val n = sorted.size

      if (n % 2 == 0)
        (f(sorted(n/2)) + f(sorted(n/2 + 1))) / (fromInt(2))
      else
        f(sorted((n+1)/2))
    }
  }
}

object Statistics extends StatisticsLow
