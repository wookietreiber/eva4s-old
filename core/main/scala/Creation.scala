package org.eva4s

/** Provides the creation of randomly generated individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P inut / problem type, represents the problem data structure
  */
trait Creator[G,P] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  /** Returns a randomly generated genome. */
  def ancestor: G

  /** Returns a randomly generated individual. */
  final def Ancestor(): Individual[G] = Individual(ancestor)

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
    * @param e $evolutionary
    * @param f $creation, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    def ancestor: G = f(problem)
  }

  /** Creates a new [[Creator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $creation
    */
  def independent[G, P](e: Evolutionary[G,P])(f: ⇒ G): Creator[G,P] = new Creator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    def ancestor: G = f
  }

}
