package org.example
package evo

import scala.util.Random

import eva4s.api.app._

object CustomizedEvoSolver extends EvolutionaryApp {

  type Genome = Double
  type Problem = Double => Double
  type F[Genome] = scalaz.Id.Id[Genome]

  val selector = PlusSelector[Genome]()

  val problem: Problem =
    (x: Double) => x * x + 4

  def fitness(genome: Genome): Double =
    problem(genome)

  def create: Genome =
    (util.Random.nextInt(10000) - 5000).toDouble

  def deviate(n: Int) =
    Random.nextInt + 1 * (if (Random.nextBoolean) 1 else -1)

  def mutate(genome: Genome) =
    genome + deviate(6)

  def pmutate(genome: Genome) =
    genome * (Random.nextDouble / 4.0 - 0.125)

  def recombine(g1: Genome, g2: Genome): F[Genome] =
    (g1 + g2) / 2

  def reporter = Reporter.Composite (
    Reporter.Console,
    ChartReporter.Line("f(x) = x^2 + 4")
  )

  val gs = 200

  implicit def sel = PlusSelector[Genome]
  implicit def mat = RandomAcceptanceMatchmaker[Genome]()
  implicit def mut = ExponentialMutagen(gs)

  def evolver = new SingleEvolver(generations = gs)

}
