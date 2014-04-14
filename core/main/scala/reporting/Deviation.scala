package eva4s
package reporting

import eva4s.util._

/** [[Reporter]] mixin to calculate a simple deviation. */
trait Deviation extends DocMacros {

  self: Reporter =>

  /** Returns a simple deviation tuple of `(fittest,average,unfittest)`.
    *
    * @param offspring $offspring
    *
    * @return a 3-tuple containing the fitness values of the fittest, average and unfittest
    *         individuals in order
    */
  final def deviation(offspring: Seq[Individual[_]]): (Double,Double,Double) = {
    val fittest = offspring.minBy(_.fitness).fitness
    val unfittest = offspring.maxBy(_.fitness).fitness
    val average = offspring.geometricMeanBy(_.fitness)

    (fittest, average, unfittest)
  }

}
