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

  def reporter: Reporter

  def main(args: Array[String]): Unit = {
    val fittest = evolver(problem)
    println(fittest)
  }

  implicit final def FitnessI = Fitness(fitness)
  implicit final def CreatorI = Creator(create)
  implicit final def MutatarI = Mutator(mutate)
  implicit final def PointMutatorI = PointMutator(pmutate)
  implicit final def RecombinatorI = Recombinator[Genome,F](recombine)
  implicit final def ReporterI = reporter

}

object EvolutionaryApp {

  abstract class Sequential extends EvolutionaryApp {
    type F[Genome] = Id[Genome]

    def generations = 200
    def pairs = 100
    def survivors = 23

    def pmutate(genome: Genome): Genome = identity(genome)

    def reporter: Reporter = Reporter.Console

    implicit def sel = selecting.PlusSelector[Genome]
    implicit def mat = matchmaking.RandomAcceptanceMatchmaker[Genome]()
    implicit def mut = mutating.ExponentialMutagen(generations)

    def evolver = new evolving.SingleEvolver[Genome,Problem](generations, survivors, pairs)
  }

  abstract class Split extends EvolutionaryApp {
    type F[Genome] = GenomeP[Genome]

    def generations = 200
    def individuals = 100

    def pmutate(genome: Genome): Genome = identity(genome)

    def reporter: Reporter = Reporter.Console

    implicit def mat = matchmaking.TournamentMatchmaker[Genome](participants = 5)
    implicit def mut = mutating.ExponentialMutagen(generations)

    def evolver = new evolving.SplitEvolver[Genome,Problem](generations, individuals)
  }

}
