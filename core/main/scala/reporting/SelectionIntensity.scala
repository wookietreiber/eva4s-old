package eva4s
package reporting

import eva4s.util._

trait SelectionIntensity {

  self: Reporter =>

  private implicit final val DoubleIntegral: Integral[Double] =
    scala.math.Numeric.DoubleAsIfIntegral

  /** Returns the selection intensity of the given generation transition. */
  final def selectionIntensity(oldGen: Seq[Individual[_]], newGen: Seq[Individual[_]]): Double = {
    val fselbar = newGen.averageBy(_.fitness)
    val fbar    = oldGen.averageBy(_.fitness)

    val product = (1.0 / (oldGen.size - 1)) * (oldGen.map(i => math.pow(fbar - i.fitness, 2)).sum)

    val sigma = math.sqrt(product)

    if (sigma != 0.0) {
      (fselbar - fbar) / sigma
    } else {
      sigma
    }
  }

}
