package eva4s
package matchmaking

import eva4s.util._

/** Returns the fittest individuals of `pairs` tournaments.
  *
  * There will be `pairs` tournaments to determine the pairs. Each tournament consists of
  * `participants` randomly chosen participants. From these participants are the fittest two for the
  * pair chosen.
  *
  * @param participants amount of randomly selected individuals attending a tournament, must be
  * greater than or equal to 2
  */
case class TournamentMatchmaker[G](participants: Int) extends Matchmaker[G] {

  require(participants >= 2, "participants must be greater than or equal to 2")

  override def apply(parents: Seq[Individual[G]], pairs: Int): Seq[IndividualP[G]] = {
    Vector.fill(pairs) {
      val winners = parents choose participants sortBy { _.fitness } take 2
      Pair(winners.head, winners.last)
    }
  }

}
