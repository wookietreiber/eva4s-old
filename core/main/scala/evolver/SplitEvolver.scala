package org.eva4s
package evolver

import scala.annotation.tailrec

import scalay.collection._

import scalaz.Functor

import Matchmaking._
import Mutagens._
import Selection._

/** An evolver that splits each generation into one part to be mutated and another part to be
  * recombined. This evolver picks always all new individuals for the next generation. This way the
  * population size always stays the same and there is no environmental selection involved in this
  * process. The chosen [[Mutagen]] determines the ratio of how many individuals recombine and how
  * many individuals mutate per generation, so the ratio should favor recombination because parental
  * selection is the driving force to improve the fitness.
  *
  * Since the recombination of two individuals has to produce another two individuals to keep the
  * population size the same you can use only [[CrossoverRecombination]] with this evolver.
  */
object SplitEvolver extends Evolver {

  /** Executes an evolutionary algorithm, standalone variant.
    *
    * @param evolutionary $evolutionary
    * @param creator $creator
    * @param mutator $mutator
    * @param pmutator $pmutator
    * @param recombinator $crossoverrecombinator
    * @param generations $generations
    * @param individuals $survivors
    * @param matchmaker $matchmaker
    * @param mutagen $mutagen
    */
  def apply[G,P](generations: Int = 200, individuals: Int = 100)
    (implicit
      evolutionary: Evolutionary[G,P],
      creator: Creation[G,P],
      mutator: Mutation[G,P],
      pmutator: PointMutation[G,P],
      recombinator: CrossoverRecombination[G,P],
      matchmaker: Matchmaker[G] = RandomAcceptanceMatchmaker[G](0.7) _,
      mutagen: Mutagen = ExponentialMutagen(generations))
      : Individual[G] = {
    import evolutionary._
    import creator.Ancestor
    import mutator.Mutant
    import pmutator.pmutate
    import recombinator.procreate

    implicit val genomePF = new Functor[GenomeP] {
      override def map[A,B](gs: GenomeP[A])(f: A ⇒ B): GenomeP[B] = (f(gs._1),f(gs._2))
    }

    def ancestors(n: Int): Seq[Individual[G]] = Vector.fill(n)(Ancestor)
    def tuple2seq[G](xs: (G,G)): Seq[G] = Seq(xs._1,xs._2)

    @tailrec
    def evolve(parents: Seq[Individual[G]], generation: Int): Individual[G] =
      if (generation == generations) {
        parents minBy { _.fitness }
      } else {
        val recombinations = (individuals * (1.0 - mutagen(generation)) / 2.0).round.toInt
        val mutations      = individuals - (2 * recombinations)

        val mutants = parents choose mutations map Mutant

        val offspring: Seq[IndividualP[G]] = for {
          (p1,p2) ← matchmaker(parents, recombinations)
          g1 = pmutate(p1.genome)
          g2 = pmutate(p2.genome)
          children = procreate(g1,g2)
        } yield children

        // no point mutation variant: val offspring = matchmaker(parents, recombinations).map(procreate).flatten(tuple2seq)

        val nextGen = mutants ++ (offspring.flatten(tuple2seq))

        evolve(parents = nextGen, generation = generation + 1)
      }

    evolve(parents = ancestors(individuals), generation = 1)
  }

}
