package eva4s
package mutating

/** A mutagen that always uses the same probability. */
case class ConstantMutagen(probability: Double) extends Mutagen {

  override def apply(generation: Int): Double =
    probability

  override def toString: String =
    s"""ConstantMutagen(probability=$probability)"""

}
