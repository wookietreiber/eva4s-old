import scalaz.Id.Id

/** This package brings evolutionary algorithms to Scala. */
package object eva4s {

  // -----------------------------------------------------------------------------------------------
  // aliases
  // -----------------------------------------------------------------------------------------------

  /** Type alias for a pair of genomes. */
  type GenomeP[G] = Pair[G,G]

  /** Type alias for a pair of individuals. */
  type IndividualP[G] = Pair[Individual[G],Individual[G]]

  /** A `Matchmaker` pairs individuals up with each other. It models parental selection.
    *
    * @tparam G the type of the genome of the individuals, represents a solution of the problem
    */
  type Matchmaker[G] = (Seq[Individual[G]],Int) => Seq[IndividualP[G]]

  /** A `Mutagen` determines the probability with which individuals mutate, depending on the current generation. */
  type Mutagen = Int => Double

  /** A `Selector` determines how the individuals for the next generation are chosen. It models
    * environmental selection.
    *
    * @tparam G the type of the genome of the individuals, represents a solution of the problem
    */
  type Selector[G] = (Seq[Individual[G]],Seq[Individual[G]]) => Seq[Individual[G]]

  /** Dummy for documentation definition inheritance.
    *
    * @define end the mutation probability at generation `generations`
    * @define generationsEvolver amount of generations until the fittest individual is chosen for as
    * the solution
    * @define generationsMutagen the final generation
    * @define genome the type of the genome of the individuals
    * @define mutagen chance of child to mutate as a function from current generation to a floating
    * point value between 0 and 1
    * @define matchmaker determines, which parents reproduce new children
    * @define mu the amount of chosen children
    * @define offspring the offspring of the generation
    * @define pairs the amount of pairs generated
    * @define parents the parents of the generation
    * @define selector determines, how the individuals for the next generation are chosen
    * @define start the mutation probability at generation zero
    * @define survivors amount of survivors per generation as well as initial population /
    * ancestors
    *
    * @define creator creation building block
    * @define mutator mutation building block
    * @define pmutator point mutation building block
    * @define recombinator recombination building block
    * @define crossoverrecombinator crossover recombination building block
    * @define onlychildrecombinator only child recombination building block
    */
  private[eva4s] trait DocDummy

  type CrossoverRecombinator[G] = Recombinator[G,GenomeP]
  type OnlyChildRecombinator[G] = Recombinator[G,Id]

  // -----------------------------------------------------------------------------------------------
  // others
  // -----------------------------------------------------------------------------------------------

  /** Returns some debugger function. */
  val printer: Option[(Int,Double,Double) => Unit] = Some { (g: Int, i: Double, f: Double ) =>
    printf("generation: %5d     selection intensity: % 1.5f     average fitness: %f\n", g, i, f)
  }

  private[eva4s] def ranks(size: Int) = Vector.tabulate(size) {
    i => 2.0 / size * (1 - i.toDouble / (size - 1.0))
  }

}
