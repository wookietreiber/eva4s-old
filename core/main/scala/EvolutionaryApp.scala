package org.eva4s

import language.higherKinds

abstract class EvolutionaryApp {
  type Genome
  type Problem
  type F[_]

  def problem: Problem

  def fitness(genome: Genome): Double

  def creator: Genome

  def mutator(genome: Genome): Genome

  def pmutator(genome: Genome): Genome

  def recombinator(g1: Genome, g2: Genome): F[Genome]

  def evolver: Evolver

  def main(args: Array[String]): Unit
}

object EvolutionaryApp {

  abstract class Sequential extends EvolutionaryApp {
    type F[_] = scalaz.Id.Id[Genome]

    val evolver = org.eva4s.evolver.SingleEvolver

    def pmutator(genome: Genome) = identity(genome)

    override final def main(args: Array[String]) = {
      implicit val e = Evolutionary.simple[Genome,Problem](problem)(fitness)
      implicit val c = Creator.independent(e)(creator)
      implicit val m = Mutator.independent(e)(mutator)
      implicit val p = PointMutator.independent(e)(pmutator)
      implicit val r = OnlyChildRecombinator.independent(e)(recombinator)

      val fittest = evolver()

      println(fittest)
    }
  }

}
