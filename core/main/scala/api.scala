package eva4s

import language.higherKinds

/** Contains eva4s APIs. */
object api {

  /** API for a self-contained application with a hard-coded configuration. */
  object app extends Imports {
    type EvolutionaryApp = eva4s.EvolutionaryApp
    val  EvolutionaryApp = eva4s.EvolutionaryApp
  }

  /** API for self-contained command line applications with configuration parsing. */
  object cli extends Imports {
  }

  /** API for REPL usage. */
  object console extends Imports {
  }

  /** Contains common imports. */
  private[api] trait Imports {
    type SingleEvolver[G,P] = evolving.SingleEvolver[G,P]
    val  SingleEvolver      = evolving.SingleEvolver

    type SplitEvolver[G,P] = evolving.SplitEvolver[G,P]
    val  SplitEvolver      = evolving.SplitEvolver

    type Reporter = eva4s.Reporter
    val  Reporter = eva4s.Reporter

    val  ChartReporter = eva4s.reporting.ChartReporter

    type StreamReporter = eva4s.reporting.StreamReporter
    val  StreamReporter = eva4s.reporting.StreamReporter

    type MultipleTournamentMatchmaker[G] = matchmaking.MultipleTournamentMatchmaker[G]
    val  MultipleTournamentMatchmaker    = matchmaking.MultipleTournamentMatchmaker

    type RandomAcceptanceMatchmaker[G] = matchmaking.RandomAcceptanceMatchmaker[G]
    val  RandomAcceptanceMatchmaker    = matchmaking.RandomAcceptanceMatchmaker

    type RandomForcedMatchmaker[G] = matchmaking.RandomForcedMatchmaker[G]
    val  RandomForcedMatchmaker    = matchmaking.RandomForcedMatchmaker

    type RankBasedMatchmaker[G] = matchmaking.RankBasedMatchmaker[G]
    val  RankBasedMatchmaker    = matchmaking.RankBasedMatchmaker

    type TournamentMatchmaker[G] = matchmaking.TournamentMatchmaker[G]
    val  TournamentMatchmaker    = matchmaking.TournamentMatchmaker

    type ConstantMutagen = mutating.ConstantMutagen
    val  ConstantMutagen = mutating.ConstantMutagen

    type ExponentialMutagen = mutating.ExponentialMutagen
    val  ExponentialMutagen = mutating.ExponentialMutagen

    type PolynomialMutagen = mutating.PolynomialMutagen
    val  PolynomialMutagen = mutating.PolynomialMutagen

    type ArithmeticCrossover[F[_]] = recombining.ArithmeticCrossover[F]
    val  ArithmeticCrossover       = recombining.ArithmeticCrossover

    type CrossoverRecombinator[G] = eva4s.CrossoverRecombinator[G]
    val  CrossoverRecombinator    = recombining.CrossoverRecombinator

    type IntermediateRecombinator[F[_]] = recombining.IntermediateRecombinator[F]
    val  IntermediateRecombinator       = recombining.IntermediateRecombinator

    type LineRecombinator[F[_]] = recombining.LineRecombinator[F]
    val  LineRecombinator       = recombining.LineRecombinator

    type OnePointCrossover[F[_],A] = recombining.OnePointCrossover[F,A]
    val  OnePointCrossover         = recombining.OnePointCrossover

    type OnlyChildRecombinator[G] = eva4s.OnlyChildRecombinator[G]
    val  OnlyChildRecombinator    = recombining.OnlyChildRecombinator

    type TwoPointCrossover[F[_],A] = recombining.TwoPointCrossover[F,A]
    val  TwoPointCrossover         = recombining.TwoPointCrossover

    type UniformCrossover[F[_],A] = recombining.UniformCrossover[F,A]
    val  UniformCrossover         = recombining.UniformCrossover

    type CommaSelector[G] = selecting.CommaSelector[G]
    val  CommaSelector    = selecting.CommaSelector

    type LambdaSelector[G] = selecting.LambdaSelector[G]
    val  LambdaSelector    = selecting.LambdaSelector

    type PlusSelector[G] = selecting.PlusSelector[G]
    val  PlusSelector    = selecting.PlusSelector

    type RandomPlusSelector[G] = selecting.RandomPlusSelector[G]
    val  RandomPlusSelector    = selecting.RandomPlusSelector
  }
}
