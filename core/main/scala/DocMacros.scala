package eva4s

/** Mixin solely for the purpose of providing common documentation macros.
  *
  * @define end the mutation probability at generation `generations`
  * @define generationsEvolver amount of generations until the fittest individual is chosen for as the solution
  * @define generationsMutagen the final generation
  * @define genome the type of the genome of the individuals
  * @define mutagen chance of child to mutate as a function from current generation to a floating point value between 0 and 1
  * @define matchmaker determines, which parents reproduce new children
  * @define mu the amount of chosen children
  * @define pairs the amount of pairs generated
  * @define selector determines, how the individuals for the next generation are chosen
  * @define start the mutation probability at generation zero
  * @define survivors amount of survivors per generation as well as initial population / ancestors
  *
  * @define parents the parents of the current generation
  * @define offspring the offspring of the current generation
  *
  * @define creator creation building block
  * @define mutator mutation building block
  * @define pmutator point mutation building block
  * @define recombinator recombination building block
  * @define crossoverrecombinator crossover recombination building block
  * @define onlychildrecombinator only child recombination building block
  */
trait DocMacros extends Any
