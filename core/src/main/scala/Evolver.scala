/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2013  Christian Krause                                                         *
 *                                                                                               *
 *  Christian Krause  <kizkizzbangbang@googlemail.com>                                           *
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

import language.higherKinds

import scala.annotation.tailrec
import scalay.collection._
import scalaz.Functor

/** An evolver is to an [[org.eva4s.Evolutionary]] what an executor is to a thread. How an
  * [[org.eva4s.Evolutionary]] is executed (sequential, parallel, distributed) depends on the actual
  * evolver implementation.
  *
  * @note An [[org.eva4s.Evolver]] implementation should be stateless.
  *
  * @see [[org.eva4s.Evolvers]] contains default implementations.
  */
trait Evolver {

  implicit val DoubleIntegral: Integral[Double] = scala.math.Numeric.DoubleAsIfIntegral

  /** Returns the selection intensity of the given generation. */
  def selectionIntensity(oldGen: Seq[Individual[_]],
                         newGen: Seq[Individual[_]]): Double = {

    val fselbar = newGen averageBy { _.fitness }
    val fbar    = oldGen averageBy { _.fitness }

    val sigma = math.sqrt(
      (1.0 / (oldGen.size - 1)) * ((oldGen map { i ⇒ math.pow(fbar - i.fitness, 2) }).sum)
    )

    if (sigma != 0.0) (fselbar - fbar) / sigma else 0.0
  }

}

/** $EvolverInfo */
object Evolvers extends Evolvers

/** $EvolverInfo
  *
  * @define EvolverInfo Contains default [[org.eva4s.Evolver]] implementations.
  */
trait Evolvers extends Matchmaking with Mutagens with Selection {

  object SingleEvolver extends Matchmaking with Mutagens with Selection {
    def apply[G,P](generations: Int = 200, survivors: Int = 23, pairs: Int = 100)
      (implicit
        evolutionary: Evolutionary[G,P],
        creator: Creation[G,P],
        mutator: Mutation[G,P],
        pmutator: PointMutation[G,P],
        recombinator: Recombination[G,P,scalaz.Id.Id],
        matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
        mutagen: Mutagen = ExponentialMutagen(generations),
        selector: Selector[G] = SurvivalOfTheFittest[G] _)
        : Individual[G] = {
      import evolutionary._
      import creator.Ancestor
      import mutator.Mutant
      import pmutator.pmutate
      import recombinator.recombine

      def ancestors(n: Int): Seq[Individual[G]] = Vector.fill(n)(Ancestor)

      @tailrec
      def evolve(parents: Seq[Individual[G]], generation: Int): Individual[G] =
        if (generation == generations) {
          parents minBy { _.fitness }
        } else {
          val offspring = for {
            (p1,p2) ← matchmaker(parents, pairs)
            genome  = recombine(pmutate(p1.genome), pmutate(p2.genome))
          } yield if (Random.nextDouble < mutagen(generation))
            Mutant(genome)
          else
            Individual(genome)

          val nextGen = selector(parents, offspring)

          // bail out if there is just one individual left
          if (nextGen.size == 1)
            return nextGen.head

          evolve(parents = nextGen, generation = generation + 1)
        }

      evolve(parents = ancestors(survivors), generation = 1)
    }

    def full[G,P](full: Full[G,P,scalaz.Id.Id])(generations: Int = 200, survivors: Int = 23, pairs: Int = 100)
      (implicit matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
        mutagen: Mutagen = ExponentialMutagen(generations),
        selector: Selector[G] = SurvivalOfTheFittest[G] _)
        : Individual[G] =
      apply(generations,survivors,pairs)(full,full,full,full,full,matchmaker,mutagen,selector)
  }

  object SplitEvolver extends Evolver {
    def apply[G,P](generations: Int = 200, individuals: Int = 100)
      (implicit
        evolutionary: Evolutionary[G,P],
        creator: Creation[G,P],
        mutator: Mutation[G,P],
        pmutator: PointMutation[G,P],
        recombinator: Recombination[G,P,GenomeP],
        matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
        mutagen: Mutagen = ExponentialMutagen(generations),
        selector: Selector[G] = SurvivalOfTheFittest[G] _)
        : Individual[G] = {
      import evolutionary._
      import creator.Ancestor
      import mutator.Mutant
      import pmutator.pmutate
      import recombinator.procreate

      implicit val genomePF = new Functor[GenomeP] {
        override def map[A,B](gs: GenomeP[A])(f: A ⇒ B): GenomeP[B] = (f(gs._1),f(gs._2))
      }

      def ancestors(n: Int): Seq[Individual[G]] = Vector.fill(n)(Ancestor)
      def tuple2seq[G](xs: (G,G)) = Seq(xs._1,xs._2)

      @tailrec
      def evolve(parents: Seq[Individual[G]], generation: Int): Individual[G] =
        if (generation == generations) {
          parents minBy { _.fitness }
        } else {
          val recombinations = (individuals * (1.0 - mutagen(generation)) / 2.0).round.toInt
          val mutations      = individuals - (2 * recombinations)

          val mutants   = parents choose mutations map Mutant
          val offspring = matchmaker(parents, recombinations).map(procreate).flatten(tuple2seq)

          val nextGen = mutants ++ offspring

          evolve(parents = nextGen, generation = generation + 1)
        }

      evolve(parents = ancestors(individuals), generation = 1)
    }
  }
}
