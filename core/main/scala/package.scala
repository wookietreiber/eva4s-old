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
