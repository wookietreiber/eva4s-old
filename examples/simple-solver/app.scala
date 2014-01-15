package org.example
package evo

import org.eva4s.api._

object EvoSolver extends EvolutionaryApp.Sequential {

  type Genome = Double
  type Problem = Double => Double

  val problem: Problem =
    (x: Double) => x * x + 4

  def fitness(genome: Genome): Double =
    problem(genome)

  def create: Genome =
    (util.Random.nextInt(10000) - 5000).toDouble

  def mutate(genome: Genome): Genome =
    genome + util.Random.nextInt(9) - 4

  def recombine(g1: Genome, g2: Genome): Genome =
    (g1 + g2) / 2

}
