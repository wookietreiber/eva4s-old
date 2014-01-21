package eva4s

import language.higherKinds

/** Executes an evolutionary algorithm. An evolver is to an evolutionary what an executor is to a
  * thread. How an evolutionary is executed (sequential, parallel, distributed) depends on the
  * actual [[Evolver]] implementation.
  *
  * @note An [[Evolver]] implementation should be stateless.
  *
  * @see [[api]] contains default implementations.
  */
trait Evolver[G,P] extends DocDummy {

  def creator: Creator[G]

  def apply(problem: P): Individual[G]

}

import util._

object Evolver {

  private[this] implicit val DoubleIntegral: Integral[Double] = scala.math.Numeric.DoubleAsIfIntegral

  /** Returns the selection intensity of the given generation. */
  def selectionIntensity(oldGen: Seq[Individual[_]], newGen: Seq[Individual[_]]): Double = {
    val fselbar = newGen averageBy { _.fitness }
    val fbar    = oldGen averageBy { _.fitness }

    val sigma = math.sqrt(
      (1.0 / (oldGen.size - 1)) * ((oldGen map { i ⇒ math.pow(fbar - i.fitness, 2) }).sum)
    )

    if (sigma != 0.0) (fselbar - fbar) / sigma else 0.0
  }

}
