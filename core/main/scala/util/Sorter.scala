package eva4s
package util

import language.higherKinds

import scala.collection.Seq

trait Sorter[M[_]] {
  def sortWith[A](xs: M[A])(lt: (A,A) => Boolean): M[A] = sort(xs)(Ordering fromLessThan lt)
  def sortBy[A,B:Ordering](xs: M[A])(f: A => B): M[A] = sort(xs)(implicitly[Ordering[B]] on f)
  def sort[A:Ordering](xs: M[A]): M[A]
}

trait SorterLow {
  implicit def SeqSorter[CC[X] <: Seq[X]]: Sorter[CC] = new Sorter[CC] {
    override def sort[A:Ordering](xs: CC[A]): CC[A] =
      xs.sorted(implicitly[Ordering[A]]).asInstanceOf[CC[A]]
  }
}

object Sorter extends SorterLow
