package org.eva4s

/** Provides the means to mutate genomes and individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  *
  * @define NewMutantInfo The purpose of this method is the convenient creation of a new mutant. Use
  * it like the factory method of a case class. This method performs the mutation on its own, it is
  * not needed to do this in advance.
  */
trait Mutator[G,P] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  /** Returns a new genome by mutating the given one. */
  def mutate(genome: G): G

  /** Returns a new individual by mutating the given genome.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(genome: G): Individual[G] = Individual(mutate(genome))

  /** Returns a new individual by mutating the genome of the given individual.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(individual: Individual[G]): Individual[G] = Mutant(individual.genome)

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

/** Factory for [[Mutator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define mutation mutation function
  */
object Mutator {

  /** Creates a new [[Mutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ G ⇒ G): Mutator[G,P] = new Mutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def mutate(genome: G): G = f(problem)(genome)
  }

  /** Creates a new [[Mutator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $mutation
    */
  def independent[G,P](e: Evolutionary[G,P])(f: G ⇒ G): Mutator[G,P] = new Mutator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def mutate(genome: G): G = f(genome)
  }

}
