package org.example.evo.app

import org.eva4s.api._

object EvoSolver extends EvolutionaryApp.Sequential {

  type Genome = Double
  type Problem = Double => Double

  val problem = (x: Double) => x * x + 4

  def fitness(genome: Genome) =
    problem(genome)

  def creator = (Random.nextInt(10000) - 5000).toDouble

  def mutator(genome: Genome) =
    genome + Random.nextInt(9) - 4

  def recombinator(g1: Genome, g2: Genome) =
    (g1 + g2) / 2

}
