package org.example
package evo

import eva4s.api.app._

object MinimalEvoSeqSolver extends EvolutionaryApp.Sequential {

  type Genome = Double
  type Problem = Double => Double

  val problem = (x: Double) => x * x + 4

  def fitness(genome: Genome) = problem(genome)

  def create = (util.Random.nextInt(10000) - 5000).toDouble

  def mutate(genome: Genome) =
    genome + util.Random.nextInt(9) - 4

  def recombine(g1: Genome, g2: Genome) =
    (g1 + g2) / 2

}

object MinimalEvoSplitSolver extends EvolutionaryApp.Split {

  type Genome = Double
  type Problem = Double => Double

  val problem = (x: Double) => x * x + 4

  def fitness(genome: Genome) = problem(genome)

  def create = (util.Random.nextInt(10000) - 5000).toDouble

  def mutate(genome: Genome) =
    genome + util.Random.nextInt(9) - 4

  def recombine(g1: Genome, g2: Genome) =
    ((g1 + g2) / 2.0,(g1 + g2) / 2.0)

}
