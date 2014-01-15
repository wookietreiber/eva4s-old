package org.eva4s
package simple

import language.higherKinds

import scala.util.Random

object Main extends App {

  type Genome = Double
  type Problem = Double => Double

  implicit val evolutionary = Evolutionary((x: Double) => x*x + 4) {
    (problem: Problem) => (genome: Double) => problem(genome)
  }

  implicit val creator = Creator.independent(evolutionary) {
    (Random.nextInt(10000) - 5000).toDouble
  }

  implicit val mutator = Mutator.independent(evolutionary) {
    (genome: Double) => genome + Random.nextInt(9) - 4
  }

  implicit val pmutator = PointMutator.independent(evolutionary) {
    (genome: Double) => genome + Random.nextInt(3) - 1
  }

  implicit val recombinator = recombining.OnlyChildRecombinator.independent(evolutionary) {
    (g1: Double, g2: Double) => (g1 + g2) / 2
  }

  println(evolving.SingleEvolver())

}
