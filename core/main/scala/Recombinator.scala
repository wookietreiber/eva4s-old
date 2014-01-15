package org.eva4s

import language.higherKinds

import scalaz.Functor

/** Provides a mechanism for recombining the genomes of individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  * @tparam F container for the offspring; declares how many genomes/individuals are created per coupling
  */
trait Recombinator[G,P,F[_]] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  /** Returns new genomes by recombining the parents. */
  def recombine(g1: G, g2: G): F[G]

  /** Returns new genomes by recombining the parents. */
  final def recombine(parents: IndividualP[G]): F[G] =
    recombine(parents._1.genome, parents._2.genome)

  /** Returns new individuals by recombining the parents. */
  final def procreate(g1: G, g2: G)(implicit f: Functor[F]): F[Individual[G]] =
    f.map(recombine(g1,g2))(g ⇒ Individual(g))

  /** Returns new individuals by recombining the parents. */
  final def procreate(parents: IndividualP[G])(implicit f: Functor[F]): F[Individual[G]] =
    f.map(recombine(parents))(g ⇒ Individual(g))

  /** Returns the problem that needs to be solved. */
  // def problem: P
  final def problem: P =
    evolutionary.problem

  /** Returns the fitness of the given genome. */
  // def fitness(genome: G): Double
  final def fitness(genome: G): Double =
    evolutionary.fitness(genome)

  /** Returns a new individual from the given genome. */
  // def Individual(genome: G): Individual[G]
  final def Individual(genome: G): Individual[G] =
    evolutionary.Individual(genome)

}
