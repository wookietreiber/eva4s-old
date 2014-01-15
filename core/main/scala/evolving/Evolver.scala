package org.eva4s
package evolving

import language.higherKinds

import scalay.collection._

/** Executes an [[Evolutionary]] algorithm. An evolver is to an [[Evolutionary]] what an executor is
  * to a thread. How an [[Evolutionary]] is executed (sequential, parallel, distributed) depends on
  * the actual [[Evolver]] implementation.
  *
  * @note An [[Evolver]] implementation should be stateless.
  *
  * @see [[api]] contains default implementations.
  *
  * @define generations amount of generations until the fittest individual is chosen for as the
  * solution
  *
  * @define survivors amount of survivors per generation as well as initial population /
  * ancestors
  *
  * @define pairs amount of pairs generated per generation
  *
  * @define matchmaker determines, which parents reproduce new children
  *
  * @define mutagen chance of child to mutate as a function from current generation to a floating
  * point value between 0 and 1
  *
  * @define selector determines, how the individuals for the next generation are chosen
  *
  * @define evolutionary recombination building block
  *
  * @define creator creation building block
  *
  * @define mutator mutation building block
  *
  * @define pmutator point mutation building block
  *
  * @define recombinator recombination building block
  *
  * @define crossoverrecombinator crossover recombination building block
  *
  * @define onlychildrecombinator only child recombination building block
  */
trait Evolver {

  private[this] implicit val DoubleIntegral: Integral[Double] = scala.math.Numeric.DoubleAsIfIntegral

  /** Returns the selection intensity of the given generation. */
  protected def selectionIntensity(oldGen: Seq[Individual[_]],
                         newGen: Seq[Individual[_]]): Double = {

    val fselbar = newGen averageBy { _.fitness }
    val fbar    = oldGen averageBy { _.fitness }

    val sigma = math.sqrt(
      (1.0 / (oldGen.size - 1)) * ((oldGen map { i ⇒ math.pow(fbar - i.fitness, 2) }).sum)
    )

    if (sigma != 0.0) (fselbar - fbar) / sigma else 0.0
  }

}
