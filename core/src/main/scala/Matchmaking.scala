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

import scalay.collection._

import scalaz._
import Scalaz._

/** $MatchmakingInfo */
object Matchmaking extends Matchmaking

/** $MatchmakingInfo
  *
  * @define MatchmakingInfo Contains default [[org.eva4s.Matchmaker]] implementations which define
  * parental selection.
  *
  * The idiomatic usage of the functions defined here is to input the parameters of the first
  * parameter list(s) and use the remaining function as a [[org.eva4s.Matchmaker]].
  *
  * @see [[org.eva4s.Matchmaker]]
  *
  * @define acceptance chance of a pair accepting the match
  * @define genome the type of the genome of the individuals
  * @define pairs the amount of pairs generated
  * @define parents the parents of the generation
  */
trait Matchmaking {

  // -----------------------------------------------------------------------
  // probabilistic matchmaking
  // -----------------------------------------------------------------------

  /** Returns a fixed amount of arbitrary pairs of individuals. This is the simplest form of
    * probabilistic matchmaking.
    *
    * @tparam G $genome
    *
    * @param parents $parents
    * @param pairs $pairs
    */
  def RandomForcedMatchmaker[G](parents: Seq[Individual[G]], pairs: Int)
                                : Seq[Pair[Individual[G],Individual[G]]] =
    Vector.fill(pairs) { parents.choosePair }

  /** Returns a varying amount of arbitrary pairs of individuals.
    *
    * @tparam G $genome
    *
    * @param acceptance $acceptance
    * @param parents $parents
    * @param pairs $pairs
    */
  def RandomAcceptanceMatchmaker[G](acceptance: Double)
                                   (parents: Seq[Individual[G]], pairs: Int)
                                    : Seq[Pair[Individual[G],Individual[G]]] =
    for (i ← 1 to pairs if Random.nextDouble < acceptance) yield parents.choosePair

  /** Returns pairs based on their rank by fitness. The fitter the individual, the higher is the
    * possibility that it gets chosen.
    *
    * @tparam G $genome
    *
    * @param parents $parents
    * @param pairs $pairs
    *
    * @todo needs performance improvement
    */
  def RankBasedMatchmaker[G](parents: Seq[Individual[G]], pairs: Int)
                             : Seq[Pair[Individual[G],Individual[G]]] = {
    val ranked = parents sortBy { - _.fitness } zip {
      ranks(parents.size).inits.drop(1).map(_.sum).toList
    }

    def choosePair(ranked: Seq[Pair[Individual[G],Double]]): (Individual[G],Individual[G]) = {
      // improve this to not use partition
      val r1 = Random.nextDouble
      val (p11,p12) = ranked partition { _._2 < r1 }
      val par1 = p11.last._1

      val r2 = Random.nextDouble
      val (p21,p22) = (p11.init ++ p12) partition { _._2 < r2 }
      val par2 = if (p21.isEmpty) p22.head._1 else p21.last._1

      par1 → par2
    }

    Vector.fill(pairs) { choosePair(ranked) }
  }

  /** Returns the fittest individuals of `pairs` tournaments.
    *
    * There will be `pairs` tournaments to determine the pairs. Each tournament consists of
    * `participants` randomly chosen participants. From these participants are the fittest two for
    * the pair chosen.
    *
    * @tparam G $genome
    *
    * @param participants amount of randomly selected individuals attending a tournament
    * @param parents $parents
    * @param pairs $pairs
    */
  def TournamentMatchmaker[G](participants: Int)
                             (parents: Seq[Individual[G]], pairs: Int)
                              : Seq[Pair[Individual[G],Individual[G]]] = {
    require(participants >= 2, "participants must be greater or equal to 2")

    Vector.fill(pairs) {
      val winners = parents choose participants sortBy { _.fitness } take 2
      Pair(winners.head, winners.last)
    }
  }

  /** Returns the fittest individuals of `pairs * parents` tournaments.
    *
    * There will be `pairs` tournaments to determine the pairs. Each tournament consists of
    * `parents` sub-tournaments. Each sub-tournament consists of `participants` randomly chosen
    * participants. The winner of a sub-tournament gets a point. The two individuals with the most
    * points are chosen. If, by chance, a single individual is the sole winner of all tournaments it
    * will pair up with the fittest individual.
    *
    * @tparam G $genome
    *
    * @param participants amount of randomly selected individuals attending a tournament
    * @param parents $parents
    * @param pairs $pairs
    */
  def MultipleTournamentMatchmaker[G](participants: Int)
                                     (parents: Seq[Individual[G]], pairs: Int)
                                      : Seq[Pair[Individual[G],Individual[G]]] = {
    def winners = parents map { parent ⇒
      val ps = Seq(parent) ++ (parents filter { _ != parent } choose participants)
      Map(Pair(ps.minBy(_.fitness), 1))
    }

    Vector.fill(pairs) {
      val ps = winners.fold(Map())(_ ⊹ _).sortBy(- _._2).take(2).map(_._1)
      ps.size match {
        case 0 ⇒ parents.choosePair
        case 1 ⇒ Pair(ps.head, parents filter { _ != ps.head } minBy { _.fitness })
        case _ ⇒ Pair(ps.head, ps.last)
      }
    }
  }

}
