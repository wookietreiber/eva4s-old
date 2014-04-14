package eva4s
package reporting

import eva4s.util._

/** [[Reporter]] mixin to calculate the selection intensity. */
trait SelectionIntensity extends DocMacros {

  self: Reporter =>

  private[reporting] implicit final val DoubleIntegral: Integral[Double] =
    scala.math.Numeric.DoubleAsIfIntegral

  /** Returns the selection intensity of the given generation transition.
    *
    * @param parents $parents
    * @param offspring $offspring
    */
  final def selectionIntensity(parents: Seq[Individual[_]], offspring: Seq[Individual[_]]): Double = {
    val fselbar = offspring.averageBy(_.fitness)
    val fbar    = parents.averageBy(_.fitness)

    val product = (1.0 / (parents.size - 1)) * (parents.map(i => math.pow(fbar - i.fitness, 2)).sum)

    val sigma = math.sqrt(product)

    if (sigma != 0.0) {
      (fselbar - fbar) / sigma
    } else {
      sigma
    }
  }

}
