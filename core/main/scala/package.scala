package org

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

  // -----------------------------------------------------------------------------------------------
  // others
  // -----------------------------------------------------------------------------------------------

  /** Returns some debugger function. */
  val printer: Option[(Int,Double,Double) ⇒ Unit] = Some { (g: Int, i: Double, f: Double ) ⇒
    printf("generation: %5d     selection intensity: % 1.5f     average fitness: %f\n", g, i, f)
  }

  private[eva4s] def ranks(size: Int) = Vector.tabulate(size) {
    i ⇒ 2.0 / size * (1 - i.toDouble / (size - 1.0))
  }

}
