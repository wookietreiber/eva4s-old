package eva4s
package recombining

import language.higherKinds

import scala.util.Random

import scalaz.Functor
import scalaz.Length
import scalaz.Unzip
import scalaz.Zip

/** One point crossover randomly selects a crossover point and interchanges the two parents at this
  * point to produce two new children.
  */
case class OnePointCrossover[F[_],A](implicit val fitness: Fitness[F[A]], F: Functor[F], L: Length[F], U: Unzip[F], Z: Zip[F])
  extends CrossoverRecombinator[F[A]] {

  override def recombine(g1: F[A], g2: F[A]): GenomeP[F[A]] = {
    val size = L.length(g1)

    val point = Random.nextInt(size - 1) + 1

    var current = 0

    val cs = Z.zipWith(g1,g2) { (gene1, gene2) ⇒
      val p = if (current < point) (gene1,gene2) else (gene2,gene1)
      current += 1
      p
    }

    U.unzip(cs)
  }

}
