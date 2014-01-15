package org.eva4s

/** Provides a mechanism for point mutation. It differs from [[Mutator]] in the aspect that it
  * mutates genomes only slightly.
  *
  * == Uses Cases ==
  *
  * - An [[evolving.Evolver]] may use point mutation just before recombination to provide more
  *   diversity.
  *
  * - point mutation may also be used to model external events / natural mutagens like gamma rays
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  *
  * @define NewMutantInfo The purpose of this method is the convenient creation of a new mutant. Use
  * it like the factory method of a case class. This method performs the mutation on its own, it is
  * not needed to do this in advance.
  */
trait PointMutator[G,P] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  /** Returns a new genome by slightly mutating the given genome. */
  def pmutate(genome: G): G

  /** Returns a new individual by point mutating the given genome.
    *
    * @note $NewMutantInfo
    */
  final def PointMutant(genome: G): Individual[G] =
    Individual(pmutate(genome))

  /** Returns a new individual by point mutating the genome of the given individual.
    *
    * @note $NewMutantInfo
    */
  final def PointMutant(individual: Individual[G]): Individual[G] =
    PointMutant(individual.genome)

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

/** Factory for [[PointMutator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define mutation point mutation function
  */
object PointMutator {

  /** Creates a new [[PointMutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def pmutate(genome: G): G = f(problem)(genome)
  }

  /** Creates a new [[PointMutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation
    */
  def independent[G,P](e: Evolutionary[G,P])(f: G ⇒ G): PointMutator[G,P] = new PointMutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def pmutate(genome: G): G = f(genome)
  }

}
