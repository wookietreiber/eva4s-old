package eva4s

/** Provides the creation of randomly generated individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  */
trait Creator[G] {

  /** Returns a randomly generated genome. */
  def ancestor: G

  /** Returns a randomly generated individual. */
  final def Ancestor(): Individual[G] =
    fitness.Individual(ancestor)

  def fitness: Fitness[G]

}

/** Factory for [[Creator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define creation creation function for generating a random genome
  */
object Creator {

  /** Creates a new [[Creator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param f $creation
    */
  def apply[G](f: => G)(implicit F: Fitness[G]): Creator[G] = new Creator[G] {
    val fitness = F
    def ancestor: G = f
  }

}
