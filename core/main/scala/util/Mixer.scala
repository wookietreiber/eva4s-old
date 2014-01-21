package eva4s
package util

import language.higherKinds

import scala.collection.Seq
import scala.util.Random

trait Mixer[M[_]] {
  def shuffle[A](xs: M[A]): M[A]
  def choose[A](xs: M[A], n: Int): M[A]
  def choosePair[A](xs: M[A]): Pair[A,A]
}

trait MixerLow {
  implicit def SeqMixer[CC[X] <: Seq[X]]: Mixer[CC] = new Mixer[CC] {
    override def shuffle[A](xs: CC[A]): CC[A] = {
      val shuffled = xs.view map { x => x → Random.nextLong } sortBy { _._2 } map { _._1 }
      shuffled.force.asInstanceOf[CC[A]]
    }
    override def choose[A](xs: CC[A], n: Int): CC[A] = shuffle(xs).take(n).asInstanceOf[CC[A]]
    override def choosePair[A](xs: CC[A]): Pair[A,A] = {
      val two = choose(xs, 2).toIndexedSeq
      Pair(two(0), two(1))
    }
  }
}

object Mixer extends MixerLow
