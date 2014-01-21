package eva4s
package mutating

/** A mutagen based on the monotonic function `f(x) = a * exp(b*x)`. */
case class ExponentialMutagen(generations: Int, start: Double = 0.8, end: Double = 0.01) extends Mutagen {

  val a = start

  val c = end / start

  override def apply(generation: Int): Double =
    a * math.pow(c, generation.toDouble / generations)

  override def toString =
    s"""ExponentialMutagen(start=$start,end=$end,generations=$generations)"""

}
