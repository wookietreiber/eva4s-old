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

import Matchmaking._
import Mutagens._
import Selection._

/** Executes an [[Evolutionary]] algorithm. An evolver is to an [[Evolutionary]] what an executor is
  * to a thread. How an [[Evolutionary]] is executed (sequential, parallel, distributed) depends on
  * the actual [[Evolver]] implementation.
  *
  * @note An [[Evolver]] implementation should be stateless.
  *
  * @see [[Evolvers]] contains default implementations.
  *
  * @define generations amount of generations until the fittest individual is chosen for as the
  * solution
  *
  * @define survivors amount of survivors per generation as well as initial population /
  * ancestors
  *
  * @define pairs amount of pairs generated per generation
  *
  * @define matchmaker determines, which parents reproduce new children
  *
  * @define mutagen chance of child to mutate as a function from current generation to a floating
  * point value between 0 and 1
  *
  * @define selector determines, how the individuals for the next generation are chosen
  *
  * @define full full implementation of evolutionary functions
  *
  * @define evolutionary recombination building block
  *
  * @define creator creation building block
  *
  * @define mutator mutation building block
  *
  * @define pmutator point mutation building block
  *
  * @define recombinator recombination building block
  *
  * @define crossoverrecombinator crossover recombination building block
  *
  * @define onlychildrecombinator only child recombination building block
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
  * @define EvolverInfo Contains default [[Evolver]] implementations.
  */
trait Evolvers {

  /** An evolver that recombines by using a fixed amount of pairs and reduces all individuals,
    * including the parent generation, to a fixed population size. Each parent is point mutated
    * before recombination and each child may be mutated by the probability given by the
    * [[Mutator]].
    *
    * Both environmental and parental selection drive this evolver, though it depends on the amount
    * of survivors and pairs in which ratio.
    */
  object SingleEvolver extends Evolver {

    /** Executes an evolutionary algorithm, standalone variant.
      *
      * @param evolutionary $evolutionary
      * @param creator $creator
      * @param mutator $mutator
      * @param pmutator $pmutator
      * @param recombinator $onlychildrecombinator
      * @param generations $generations
      * @param survivors $survivors
      * @param pairs $pairs
      * @param matchmaker $matchmaker
      * @param mutagen $mutagen
      * @param selector $selector
      */
    def apply[G,P](generations: Int = 200, survivors: Int = 23, pairs: Int = 100)
      (implicit
        evolutionary: Evolutionary[G,P],
        creator: Creation[G,P],
        mutator: Mutation[G,P],
        pmutator: PointMutation[G,P],
        recombinator: OnlyChildRecombination[G,P],
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
          val mutprob = mutagen(generation)

          val offspring = for {
            (p1,p2) ← matchmaker(parents, pairs)
            genome  = recombine(pmutate(p1.genome), pmutate(p2.genome))
          } yield if (Random.nextDouble < mutprob) Mutant(genome) else Individual(genome)

          val nextGen = selector(parents, offspring)

          // bail out if there is just one individual left
          if (nextGen.size == 1)
            return nextGen.head

          evolve(parents = nextGen, generation = generation + 1)
        }

      evolve(parents = ancestors(survivors), generation = 1)
    }

    /** Executes an evolutionary algorithm, full variant.
      *
      * @param f $full
      * @param generations $generations
      * @param survivors $survivors
      * @param pairs $pairs
      * @param matchmaker $matchmaker
      * @param mutagen $mutagen
      * @param selector $selector
      */
    def full[G,P](f: Full[G,P,scalaz.Id.Id])(generations: Int = 200, survivors: Int = 23, pairs: Int = 100)
      (implicit matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
        mutagen: Mutagen = ExponentialMutagen(generations),
        selector: Selector[G] = SurvivalOfTheFittest[G] _)
        : Individual[G] =
      apply(generations,survivors,pairs)(f,f,f,f,f,matchmaker,mutagen,selector)
  }

  /** An evolver that splits each generation into one part to be mutated and another part to be
    * recombined. This evolver picks always all new individuals for the next generation. This way
    * the population size always stays the same and there is no environmental selection involved in
    * this process. The chosen [[Mutagen]] determines the ratio of how many individuals recombine
    * and how many individuals mutate per generation, so the ratio should favor recombination
    * because parental selection is the driving force to improve the fitness.
    *
    * Since the recombination of two individuals has to produce another two individuals to keep the
    * population size the same you can use only [[CrossoverRecombination]] with this evolver.
    */
  object SplitEvolver extends Evolver {

    /** Executes an evolutionary algorithm, standalone variant.
      *
      * @param evolutionary $evolutionary
      * @param creator $creator
      * @param mutator $mutator
      * @param pmutator $pmutator
      * @param recombinator $crossoverrecombinator
      * @param generations $generations
      * @param individuals $survivors
      * @param matchmaker $matchmaker
      * @param mutagen $mutagen
      */
    def apply[G,P](generations: Int = 200, individuals: Int = 100)
      (implicit
        evolutionary: Evolutionary[G,P],
        creator: Creation[G,P],
        mutator: Mutation[G,P],
        pmutator: PointMutation[G,P],
        recombinator: CrossoverRecombination[G,P],
        matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
        mutagen: Mutagen = ExponentialMutagen(generations))
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
      def tuple2seq[G](xs: (G,G)): Seq[G] = Seq(xs._1,xs._2)

      @tailrec
      def evolve(parents: Seq[Individual[G]], generation: Int): Individual[G] =
        if (generation == generations) {
          parents minBy { _.fitness }
        } else {
          val recombinations = (individuals * (1.0 - mutagen(generation)) / 2.0).round.toInt
          val mutations      = individuals - (2 * recombinations)

          val mutants = parents choose mutations map Mutant

          val offspring: Seq[IndividualP[G]] = for {
            (p1,p2) ← matchmaker(parents, recombinations)
            g1 = pmutate(p1.genome)
            g2 = pmutate(p2.genome)
            children = procreate(g1,g2)
          } yield children

// no point mutation variant: val offspring = matchmaker(parents, recombinations).map(procreate).flatten(tuple2seq)

          val nextGen = mutants ++ (offspring.flatten(tuple2seq))

          evolve(parents = nextGen, generation = generation + 1)
        }

      evolve(parents = ancestors(individuals), generation = 1)
    }

    /** Executes an evolutionary algorithm, full variant.
      *
      * @param f $full
      * @param generations $generations
      * @param individuals $survivors
      * @param matchmaker $matchmaker
      * @param mutagen $mutagen
      */
    def full[G,P](f: Full[G,P,GenomeP])(generations: Int = 200, individuals: Int = 100)
      (implicit matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
        mutagen: Mutagen = ExponentialMutagen(generations))
        : Individual[G] =
      apply(generations,individuals)(f,f,f,f,f,matchmaker,mutagen)

  }

}
