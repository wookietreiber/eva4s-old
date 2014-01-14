package org.eva4s
package evolver

import scala.annotation.tailrec

import Matchmaking._
import Mutagens._
import Selection._

/** An evolver that recombines by using a fixed amount of pairs and reduces all individuals,
  * including the parent generation, to a fixed population size. Each parent is point mutated before
  * recombination and each child may be mutated by the probability given by the [[Mutator]].
  *
  * Both environmental and parental selection drive this evolver, though it depends on the amount of
  * survivors and pairs in which ratio.
  */
object SingleEvolver extends Evolver {

  /** Executes an evolutionary algorithm, standalone variant.
    *
    * @param evolutionary $evolutionary
    * @param creator $creator
    * @param mutator $mutator
    * @param pmutator $pmutator
    * @param recombinator $onlychildrecombinator
    * @param generations $generations
    * @param survivors $survivors
    * @param pairs $pairs
    * @param matchmaker $matchmaker
    * @param mutagen $mutagen
    * @param selector $selector
    */
  def apply[G,P](generations: Int = 200, survivors: Int = 23, pairs: Int = 100)
    (implicit
      evolutionary: Evolutionary[G,P],
      creator: Creation[G,P],
      mutator: Mutation[G,P],
      pmutator: PointMutation[G,P],
      recombinator: OnlyChildRecombination[G,P],
      matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
      mutagen: Mutagen = ExponentialMutagen(generations),
      selector: Selector[G] = SurvivalOfTheFittest[G] _)
      : Individual[G] = {
    import evolutionary._
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
          (p1,p2) ← matchmaker(parents, pairs)
          genome  = recombine(pmutate(p1.genome), pmutate(p2.genome))
        } yield if (Random.nextDouble < mutprob) Mutant(genome) else Individual(genome)

        val nextGen = selector(parents, offspring)

        // bail out if there is just one individual left
        if (nextGen.size == 1)
          return nextGen.head

        evolve(parents = nextGen, generation = generation + 1)
      }

    evolve(parents = ancestors(survivors), generation = 1)
  }

}
