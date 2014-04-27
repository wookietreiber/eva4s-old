package eva4s
package mutating

/** A mutagen based on the monotonic function `f(x) = a * exp(b*x)`.
  *
  * @param generations the amount of intended generations
  * @param start the probability for the first generation
  * @param end the probability for the final generation
  */
case class ExponentialMutagen(generations: Int, start: Double = 0.8, end: Double = 0.01) extends Mutagen {

  private val a = start

  private val c = end / start

  override def apply(generation: Int): Double =
    a * math.pow(c, generation.toDouble / generations)

  override def toString =
    s"""ExponentialMutagen(start=$start,end=$end,generations=$generations)"""

}
