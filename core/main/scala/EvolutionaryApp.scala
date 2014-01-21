package eva4s

import language.higherKinds

import scalaz.Id.Id

abstract class EvolutionaryApp {
  type Genome
  type Problem
  type F[Genome]

  def problem: Problem

  def fitness(genome: Genome): Double

  def create: Genome

  def mutate(genome: Genome): Genome

  def pmutate(genome: Genome): Genome

  def recombine(g1: Genome, g2: Genome): F[Genome]

  def evolver: Evolver[Genome,Problem]

  def logger: Logger

  def reporter: Reporter

  def main(args: Array[String]): Unit = {
    val fittest = evolver(problem)
    println(fittest)
  }
}

object EvolutionaryApp {

  abstract class Sequential extends EvolutionaryApp {
    type F[_] = Id[Genome]

    def generations = 200
    def pairs = 100
    def survivors = 23

    implicit def f = Fitness(fitness)
    implicit def c = Creator(create)
    implicit def m = Mutator(mutate)
    implicit def p = PointMutator(pmutate)
    implicit def r = Recombinator[Genome,Id](recombine)
    implicit def sel = selecting.PlusSelector[Genome]
    implicit def mat = matchmaking.RandomAcceptanceMatchmaker[Genome]()
    implicit def mut = mutating.ExponentialMutagen(generations)

    def evolver = new evolving.SingleEvolver[Genome,Problem](generations, survivors, pairs)

    def pmutate(genome: Genome) =
      identity(genome)

    def logger = Logger.None
    def reporter = Reporter.None
  }

  abstract class Split extends EvolutionaryApp {

    def generations: Int

    implicit def matchmaker[G] = matchmaking.RandomAcceptanceMatchmaker[G]()
    implicit def mutagen = mutating.ExponentialMutagen(generations)

  }

}
