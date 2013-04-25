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
package ng

trait Evolutionary[Genome, Problem] {
  def problem: Problem
  def fitness(genome: Genome): Double

  final def Individual(genome: Genome): Individual[Genome] = new Individual(genome, fitness(genome))
}

object Evolutionary {
  def apply[G,P](p: P)(f: P ⇒ G ⇒ Double): Evolutionary[G,P] = new Evolutionary[G,P] {
    override val problem = p
    override def fitness(g: G) = f(problem)(g)
  }

  def simple[G,P](p: P)(f: G ⇒ Double): Evolutionary[G,P] = new Evolutionary[G,P] {
    override val problem = p
    override def fitness(g: G) = f(g)
  }
}

trait Creation[G,P] {
  def ancestor: G
  def problem: P
  def fitness(genome: G): Double
  def Individual(genome: G): Individual[G]
  final def Ancestor: Individual[G] = Individual(ancestor)
}

trait Creator[G,P] extends Creation[G,P] {
  def evolutionary: Evolutionary[G,P]
  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)
}

object Creator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    def ancestor: G = f(problem)
  }

  def unbiased[G, P](ep: Evolutionary[G,P])(f: ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    def ancestor: G = f
  }
}

trait Mutation[G,P] {
  def mutate(genome: G): G
  def problem: P
  def fitness(genome: G): Double
  def Individual(genome: G): Individual[G]
  final def Mutant(genome: G): Individual[G] = Individual(mutate(genome))
}

trait Mutator[G,P] extends Mutation[G,P] {
  def evolutionary: Evolutionary[G,P]
  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)
}

object Mutator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ G ⇒ G): Mutator[G,P] = new Mutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def mutate(genome: G): G = f(problem)(genome)
  }

  def unbiased[G,P](ep: Evolutionary[G,P])(f: G ⇒ G): Mutator[G,P] = new Mutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def mutate(genome: G): G = f(genome)
  }
}

trait PointMutation[G,P] {
  def pmutate(genome: G): G
  def problem: P
  def fitness(genome: G): Double
  def Individual(genome: G): Individual[G]
  final def PointMutant(genome: G): Individual[G] = Individual(pmutate(genome))
}

trait PointMutator[G,P] extends PointMutation[G,P] {
  def evolutionary: Evolutionary[G,P]
  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)
}

object PointMutator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def pmutate(genome: G): G = f(problem)(genome)
  }

  def unbiased[G,P](ep: Evolutionary[G,P])(f: G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def pmutate(genome: G): G = f(genome)
  }
}

import language.higherKinds

trait Recombination[G,P,M[_]] {
  def recombine(g1: G, g2: G): M[G]
  def problem: P
  def fitness(genome: G): Double
  def Individual(genome: G): Individual[G]
/*
  final def recombine(parents: Pair[Individual[G],Individual[G]]): M[G] =
    recombine(parents._1.genome, parents._2.genome)
  final def procreate(parents: Pair[Individual[G],Individual[G]]): Seq[Individual[G]] =
    for (genome ← recombine(parents)) yield evolutionary.Individual(genome)
*/
}

trait Recombinator[G,P,M[_]] extends Recombination[G,P,M] {
  def evolutionary: Evolutionary[G,P]
  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)
}

trait SingleChildRecombination[G,P] extends Recombinator[G,P,scalaz.Id.Id] {
}

trait SingleChildRecombinator[G,P] extends Recombinator[G,P,scalaz.Id.Id] {
}

object SingleChildRecombinator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ (G,G) ⇒ G) = new SingleChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): G = f(evolutionary.problem)(g1,g2)
  }

  def unbiased[G,P](ep: Evolutionary[G,P])(f: (G,G) ⇒ G) = new SingleChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): G = f(g1,g2)
  }
}

trait CrossoverRecombination[G,P] extends Recombination[G,P,GenomeP] {
}

trait CrossoverRecombinator[G,P] extends Recombinator[G,P,GenomeP] {
}

object CrossoverRecombinator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ (G,G) ⇒ (G,G)) = new CrossoverRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): (G,G) = f(evolutionary.problem)(g1,g2)
  }

  def unbiased[G,P](ep: Evolutionary[G,P])(f: (G,G) ⇒ (G,G)) = new CrossoverRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): (G,G) = f(g1,g2)
  }
}


trait Full[G,P,M[_]] extends Evolutionary[G,P]
    with Creation[G,P]
    with Mutation[G,P]
    with PointMutation[G,P]
    with Recombination[G,P,M]


import scala.annotation.tailrec
import scalay.collection._

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


object Main extends App {
  type Genome = Double
  type Problem = Double ⇒ Double

  implicit val evolutionary = Evolutionary((x: Double) ⇒ x*x + 4) {
    (problem: Problem) ⇒ (genome: Double) ⇒ problem(genome)
  }

  implicit val creator = Creator.unbiased(evolutionary) {
    (Random.nextInt(10000) - 5000).toDouble
  }

  implicit val mutator = Mutator.unbiased(evolutionary) {
    (genome: Double) ⇒ genome + Random.nextInt(9) - 4
  }

  implicit val pmutator = PointMutator.unbiased(evolutionary) {
    (genome: Double) ⇒ genome + Random.nextInt(3) - 1
  }

  implicit val recombinator = SingleChildRecombinator.unbiased(evolutionary) {
    (g1: Double, g2: Double) ⇒ (g1 + g2) / 2
  }

  println(SingleEvolver())
}

object MainFull extends App {
  type G = Double
  type P = Double ⇒ Double

  val full = new Full[G,P,scalaz.Id.Id] {
    def problem = (x: Double) ⇒ x*x + 4
    def fitness(genome: Double) = problem(genome)

    def ancestor = (Random.nextInt(10000) - 5000).toDouble
    def mutate(genome: Double) = genome + Random.nextInt(9) - 4
    def pmutate(genome: Double) = genome + Random.nextInt(3) - 1
    def recombine(g1: Double, g2: Double) = (g1 + g2) / 2
  }

  println(SingleEvolver.full(full)())
}
