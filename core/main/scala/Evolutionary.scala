package org.eva4s

import language.higherKinds

/** Provides the basic functions of an evolutionary algorithm.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  *
  */
trait Evolutionary[G,P] {

  /** Returns the problem that needs to be solved. */
  def problem: P

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome.
    *
    * @note The purpose of this method is the convenient creation of a new individual. It is just a
    * convenience wrapper around the case class to automatically inject the fitness according to
    * this evolutionary algorithm. Use it like the factory method of a case class.
    */
  final def Individual(genome: G): Individual[G] = new Individual(genome, fitness(genome))

}

/** Factory for standalone [[Evolutionary]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problemT input / problem type, represents the problem data structure
  * @define problem the problem to solve
  * @define fitness fitness function
  */
object Evolutionary {

  /** Creates a new [[Evolutionary]].
    *
    * @tparam G $genome
    * @tparam P $problemT
    *
    * @param p $problem
    * @param f $fitness, depending on the problem
    */
  def apply[G,P](p: P)(f: P ⇒ G ⇒ Double): Evolutionary[G,P] = new Evolutionary[G,P] {
    override val problem: P = p
    override def fitness(g: G): Double = f(problem)(g)
  }

  /** Creates a new [[Evolutionary]].
    *
    * @tparam G $genome
    * @tparam P $problemT
    *
    * @param p $problem
    * @param f $fitness
    */
  def simple[G,P](p: P)(f: G ⇒ Double): Evolutionary[G,P] = new Evolutionary[G,P] {
    override val problem: P = p
    override def fitness(g: G): Double = f(g)
  }

}
