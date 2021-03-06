package tsp

import eva4s.api.app._

object EvolutionaryTSPApp extends EvolutionaryApp.Sequential with TravelingSalesmanProblem[Int] {

  implicit val manifest = implicitly[Manifest[Int]]

  type Genome = G[Int]
  type Problem = P[Int]

  val problem: Problem = util.graph.completeWUnDiGraph (
    nodes = 1 to 5,
    maxWeight = 5
  )

  println(s"""complete, weighted, undirected input graph:\n  $problem""")

}
