package eva4s

/** Provides the means to mutate genomes and individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  *
  * @define NewMutantInfo The purpose of this method is the convenient creation of a new mutant. Use
  * it like the factory method of a case class. This method performs the mutation on its own, it is
  * not needed to do this in advance.
  */
trait Mutator[G] {

  /** Returns a new genome by mutating the given one. */
  def mutate(genome: G): G

  /** Returns a new individual by mutating the given genome.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(genome: G): Individual[G] =
    fitness.Individual(mutate(genome))

  /** Returns a new individual by mutating the genome of the given individual.
    *
    * @note $NewMutantInfo
    */
  final def Mutant(individual: Individual[G]): Individual[G] =
    Mutant(individual.genome)

  def fitness: Fitness[G]

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
    * @param f $mutation
    */
  def apply[G](f: G => G)(implicit F: Fitness[G]): Mutator[G] = new Mutator[G] {
    val fitness = F
    def mutate(genome: G): G = f(genome)
  }

}
