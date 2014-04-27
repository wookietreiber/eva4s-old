package eva4s

import language.higherKinds

package object util {

  implicit class MA[M[_],A](val value: M[A]) extends AnyVal {

    // ---------------------------------------------------------------------------------------------
    // sorting
    // ---------------------------------------------------------------------------------------------

    def sortWith(lt: (A,A) => Boolean)(implicit sorter: Sorter[M]): M[A] =
      sorted(Ordering fromLessThan lt, sorter)

    def sortBy[B](f: A => B)(implicit ord: Ordering[B], sorter: Sorter[M]): M[A] =
      sorted(ord on f, sorter)

    def sorted(implicit ord: Ordering[A], sorter: Sorter[M]): M[A] =
      sorter.sort(value)

    // ---------------------------------------------------------------------------------------------
    // shuffling
    // ---------------------------------------------------------------------------------------------

    def shuffle(implicit mixer: Mixer[M]): M[A] =
      mixer.shuffle(value)

    def choose(n: Int)(implicit mixer: Mixer[M]): M[A] =
      mixer.choose(value, n)

    def choosePair(implicit mixer: Mixer[M]): (A,A) =
      mixer.choosePair(value)

    // ---------------------------------------------------------------------------------------------
    // statistical
    // ---------------------------------------------------------------------------------------------

    def arithmeticMean(implicit stats: Statistics[M], int: Integral[A]): A =
      stats.arithmeticMean(value)

    def arithmeticMeanBy[B](f: A => B)(implicit stats: Statistics[M], int: Integral[B]): B =
      stats.arithmeticMeanBy(value)(f)

    def average(implicit stats: Statistics[M], int: Integral[A]): A =
      stats.arithmeticMean(value)

    def averageBy[B](f: A => B)(implicit stats: Statistics[M], int: Integral[B]): B =
      stats.arithmeticMeanBy(value)(f)

    def geometricMean(implicit stats: Statistics[M], num: Numeric[A]): Double =
      stats.geometricMean(value)

    def geometricMeanBy[B](f: A => B)(implicit stats: Statistics[M], num: Numeric[B]): Double =
      stats.geometricMeanBy(value)(f)

    def harmonicMean(implicit stats: Statistics[M], int: Integral[A]): A =
      stats.harmonicMean(value)

    def harmonicMeanBy[B](f: A => B)(implicit stats: Statistics[M], int: Integral[B]): B =
      stats.harmonicMeanBy(value)(f)

    def quadraticMean(implicit stats: Statistics[M], num: Numeric[A]): Double =
      stats.quadraticMean(value)

    def quadraticMean[B](f: A => B)(implicit stats: Statistics[M], num: Numeric[B]): Double =
      stats.quadraticMeanBy(value)(f)

    def median(implicit stats: Statistics[M], sorter: Sorter[M], int: Integral[A]): A =
      stats.median(value)

    def medianBy[B](f: A => B)(implicit stats: Statistics[M], sorter: Sorter[M], int: Integral[B]): B =
      stats.medianBy(value)(f)

  }

}
