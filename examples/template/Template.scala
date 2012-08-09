package org.eva4s
package foo

object Foo {
  type Genome  = Int
  type Problem = Int
}

import Foo._

class Foo(val problem: Problem) extends Evolutionary[Genome,Problem] {

  override def ancestor: Genome =
    sys error "unimplemented"

  override def fitness(genome: Genome): Double =
    sys error "unimplemented"

  override def mutate(genome: Genome): Genome =
    sys error "unimplemented"

  override def recombine(p1: Genome, p2: Genome): Seq[Genome] =
    sys error "unimplemented"

}
