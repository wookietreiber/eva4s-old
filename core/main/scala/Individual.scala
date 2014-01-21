package eva4s

/** Represents a candidate solution of a problem and its fitness.
  *
  * == Genome ==
  *
  * Individuals carry genetic information, the [[Individual!.genome genome]]. The genome directly
  * represents a candidate solution to a problem.
  *
  * == Fitness ==
  *
  * The [[Individual!.fitness fitness]] of an individual, in biology, describes its ability to both
  * survive and reproduce. In the context of evolutionary algorithms it serves as a value describing
  * how optimal a candidate solution is for solving a given problem. The fitness of an individual is
  * used by both [[selecting environmental selection]] and [[matchmaking parental selection]].
  *
  * @tparam G the type of the genome of the individuals
  *
  * @param genome Returns the genome of this individual.
  * @param fitness Returns the fitness of this individual.
  */
case class Individual[G] private[eva4s] (genome: G, fitness: Double)
