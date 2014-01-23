package eva4s
package evolving

import scala.annotation.tailrec
import scala.util.Random

import scalaz.Id.Id

/** An evolver that recombines by using a fixed amount of pairs and reduces all individuals,
  * including the parent generation, to a fixed population size. Each parent is point mutated before
  * recombination and each child may be mutated by the probability given by the [[Mutator]].
  *
  * Both environmental and parental selection drive this evolver, though it depends on the amount of
  * survivors and pairs in which ratio.
  */
class SingleEvolver[G,P](generations: Int = 200, survivors: Int = 23, pairs: Int = 100)
  (implicit
    fitness: Fitness[G],
    val creator: Creator[G],
    mutator: Mutator[G],
    pmutator: PointMutator[G],
    recombinator: Recombinator[G,Id],
    selector: Selector[G],
    matchmaker: Matchmaker[G],
    mutagen: Mutagen,
    val reporter: Reporter)
    extends Evolver[G,P] {

  def apply(problem: P): Individual[G] = {
    import fitness.Individual
    import creator.Ancestor
    import mutator.Mutant
    import pmutator.pmutate
    import recombinator.recombine

    def ancestors(n: Int): Seq[Individual[G]] = Vector.fill(n)(Ancestor)

    @tailrec
    def evolve(parents: Seq[Individual[G]], generation: Int): Individual[G] =
      if (generation == generations) {
        parents minBy { _.fitness }
      } else {
        val mutprob = mutagen(generation)

        val offspring = for {
          (p1,p2) <- matchmaker(parents, pairs)
          genome  = recombine(pmutate(p1.genome), pmutate(p2.genome))
        } yield if (Random.nextDouble < mutprob) Mutant(genome) else Individual(genome)

        val nextGen = selector(parents, offspring)

        // bail out if there is just one individual left
        if (nextGen.size == 1)
          return nextGen.head

        reporter.report(generation, parents, nextGen)
        evolve(parents = nextGen, generation = generation + 1)
      }

    evolve(parents = ancestors(survivors), generation = 1)
  }

}
