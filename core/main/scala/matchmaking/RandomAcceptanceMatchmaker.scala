package eva4s
package matchmaking

import scala.util.Random

import eva4s.util._

/** Returns a varying amount of arbitrary pairs of individuals.
  *
  * @param acceptance chance of a pair accepting the match
  */
case class RandomAcceptanceMatchmaker[G](acceptance: Double = 0.7) extends Matchmaker[G] {

  override def apply(parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] =
    for (i <- 1 to pairs if Random.nextDouble < acceptance) yield
      parents.choosePair

}
