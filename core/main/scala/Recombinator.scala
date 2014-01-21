package eva4s

import language.higherKinds

import scalaz.Functor

/** Provides a mechanism for recombining the genomes of individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  * @tparam F container for the offspring; declares how many genomes/individuals are created per coupling
  */
trait Recombinator[G,F[_]] {

  /** Returns new genomes by recombining the parents. */
  def recombine(g1: G, g2: G): F[G]

  /** Returns new genomes by recombining the parents. */
  final def recombine(parents: IndividualP[G]): F[G] =
    recombine(parents._1.genome, parents._2.genome)

  /** Returns new individuals by recombining the parents. */
  final def procreate(g1: G, g2: G)(implicit f: Functor[F]): F[Individual[G]] =
    f.map(recombine(g1,g2))(g => fitness.Individual(g))

  /** Returns new individuals by recombining the parents. */
  final def procreate(parents: IndividualP[G])(implicit f: Functor[F]): F[Individual[G]] =
    f.map(recombine(parents))(g => fitness.Individual(g))

  def fitness: Fitness[G]

}

object Recombinator {

  def apply[G,F[_]](f: (G,G) => F[G])(implicit fit: Fitness[G]): Recombinator[G,F] = new Recombinator[G,F] {
    val fitness = fit
    def recombine(g1: G, g2: G): F[G] = f(g1,g2)
  }

}
