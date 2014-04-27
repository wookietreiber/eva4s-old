package eva4s
package mutating

/** A mutagen based on the monotonic function `f(x) = a + b * pow(x,degree)`.
  *
  * @param degree the polynomial degree of the function
  * @param generations the amount of intended generations
  * @param start the probability for the first generation
  * @param end the probability for the final generation
  */
case class PolynomialMutagen(degree: Double, generations: Int, start: Double = 0.8, end: Double = 0.01) extends Mutagen {

  private val a = start

  private val b = ((end - start) / math.pow(generations, degree))

  override def apply(generation: Int): Double =
    a + b * math.pow(generation, degree)

  override def toString = degree match {
    case 1.0 => s"""LinearMutagen(start=$start,end=$end,generations=$generations)"""
    case 2.0 => s"""QuadraticMutagen(start=$start,end=$end,generations=$generations)"""
    case 3.0 => s"""CubicMutagen(start=$start,end=$end,generations=$generations)"""
    case degree => s"""PolynomialMutagen(degree=$degree,start=$start,end=$end,generations=$generations)"""
  }

}
