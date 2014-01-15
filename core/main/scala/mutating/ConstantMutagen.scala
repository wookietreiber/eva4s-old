package org.eva4s
package mutating

object ConstantMutagen extends DocDummy {

  /** Returns a `Mutagen` that always uses the same probability.
    *
    * @param probability the constant mutation probability
    */
  def apply(probability: Double): Mutagen = new Function[Int,Double] {
    override def apply(generation: Int): Double =
      probability

    override def toString: String =
      "ConstantMutagen(probability=%s)".format(probability)
  }

}
