package eva4s
package mutating

/** A mutagen based on the monotonic function `f(x) = a + b * pow(x,degree)`. */
case class PolynomialMutagen(degree: Double, generations: Int, start: Double = 0.8, end: Double = 0.01) extends Mutagen {

  val a = start

  val b = ((end - start) / math.pow(generations, degree))

  override def apply(generation: Int): Double =
    a + b * math.pow(generation, degree)

  override def toString = degree match {
    case 1.0 => s"""LinearMutagen(start=$start,end=$end,generations=$generations)"""
    case 2.0 => s"""QuadraticMutagen(start=$start,end=$end,generations=$generations)"""
    case 3.0 => s"""CubicMutagen(start=$start,end=$end,generations=$generations)"""
    case degree => s"""PolynomialMutagen(degree=$degree,start=$start,end=$end,generations=$generations)"""
  }

}
