package org.eva4s

private[eva4s] trait Imports {
  type Random = scala.util.Random
  val  Random = scala.util.Random
}

object api extends Imports {
  type EvolutionaryApp = org.eva4s.EvolutionaryApp
  val  EvolutionaryApp = org.eva4s.EvolutionaryApp

  val SingleEvolver = org.eva4s.evolver.SingleEvolver
  val SplitEvolver  = org.eva4s.evolver.SplitEvolver
}
