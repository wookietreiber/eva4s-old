package org.eva4s

/** Represents a candidate solution of a problem and its fitness.
  *
  * == Genome ==
  *
  * Individuals carry genetic information, the [[Individual!.genome genome]]. The genome directly
  * represents a candidate solution to a [[Evolutionary!.problem problem]].
  *
  * == Fitness ==
  *
  * The [[Individual!.fitness fitness]] of an individual, in biology, describes its ability to both
  * survive and reproduce. In the context of evolutionary algorithms it serves as a value describing
  * how optimal a candidate solution is for solving a given [[Evolutionary!.problem problem]]. The
  * fitness of an individual is used by both [[Selection environmental selection]] and
  * [[Matchmaking parental selection]].
  *
  * @tparam G the type of the genome of the individuals
  *
  * @param genome Returns the genome of this individual.
  * @param fitness Returns the fitness of this individual.
  */
case class Individual[G](genome: G, fitness: Double)
