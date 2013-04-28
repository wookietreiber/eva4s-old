package org.eva4s
package foo

object Foo {
  type Genome  = Int
  type Problem = Int
}

import Foo._

class Foo(val problem: Problem) extends Full[Genome,Problem,GenomeP] {

  override def ancestor: Genome = ???

  override def fitness(genome: Genome): Double = ???

  override def mutate(genome: Genome): Genome = ???

  override def pmutate(genome: Genome): Genome = ???

  override def recombine(g1: Genome, g2: Genome): (Genome,Genome) = ???

}
