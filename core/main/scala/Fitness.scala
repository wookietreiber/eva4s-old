package eva4s

trait Fitness[G] {

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome.
    *
    * @note The purpose of this method is the convenient creation of a new individual. It is just a
    * convenience wrapper around the case class to automatically inject the fitness according to
    * this evolutionary algorithm. Use it like the factory method of a case class.
    */
  final def Individual(genome: G): Individual[G] =
    new Individual(genome, fitness(genome))

}

object Fitness {

  def apply[G](f: G => Double): Fitness[G] = new Fitness[G] {
    override def fitness(g: G): Double = f(g)
  }

}
