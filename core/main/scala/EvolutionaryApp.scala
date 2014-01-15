package org.eva4s

import language.higherKinds

abstract class EvolutionaryApp {
  type Genome
  type Problem
  type F[_]

  def problem: Problem

  def fitness(genome: Genome): Double

  def create: Genome

  def mutate(genome: Genome): Genome

  def pmutate(genome: Genome): Genome

  def recombine(g1: Genome, g2: Genome): F[Genome]

  def evolver: evolving.Evolver

  def main(args: Array[String]): Unit
}

object EvolutionaryApp {

  abstract class Sequential extends EvolutionaryApp {
    type F[_] = scalaz.Id.Id[Genome]

    val evolver = evolving.SingleEvolver

    def pmutate(genome: Genome) = identity(genome)

    override final def main(args: Array[String]) = {
      implicit val e = Evolutionary.simple[Genome,Problem](problem)(fitness)
      implicit val c = Creator.independent(e)(create)
      implicit val m = Mutator.independent(e)(mutate)
      implicit val p = PointMutator.independent(e)(pmutate)
      implicit val r = recombining.OnlyChildRecombinator.independent(e)(recombine)

      val fittest = evolver()

      println(fittest)
    }
  }

}
