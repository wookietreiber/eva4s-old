package ea
package foo

object Foo {
  type Genome  = Int
  type Problem = Int
}

import Foo._

class Foo(override val problem: Problem)
  extends EvolutionaryAlgorithm[Genome,Problem] {

  override def ancestor: Genome =
    sys error "unimplemented"

  override def fitness(genome: Genome): Double =
    sys error "unimplemented"

  override def mutate(genome: Genome): Genome =
    sys error "unimplemented"

  override def recombine(parents: Pair[Genome,Genome]): Iterable[Genome] =
    sys error "unimplemented"

}
