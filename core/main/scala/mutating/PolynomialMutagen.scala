package org.eva4s
package mutating

object PolynomialMutagen extends DocDummy {

  /** Returns a monotonic function based on `f(x) = a + b * pow(x,degree)`.
    *
    * @param degree the degree of the polynom
    * @param start $start
    * @param end $end
    * @param generations $generations
    */
  def apply(degree: Double, generations: Int, start: Double = 0.8, end: Double = 0.01): Mutagen = new Function[Int,Double] {
    val a = start
    val b = ((end - start) / math.pow(generations, degree))

    override def apply(generation: Int): Double = a + b * math.pow(generation, degree)

    override def toString = degree match {
      case 1.0 ⇒ "LinearMutagen(start=%s,end=%s,generations=%s)".format(
        start, end, generations
      )
      case 2.0 ⇒ "QuadraticMutagen(start=%s,end=%s,generations=%s)".format(
        start, end, generations
      )
      case 3.0 ⇒ "CubicMutagen(start=%s,end=%s,generations=%s)".format(
        start, end, generations
      )
      case degree ⇒ "PolynomialMutagen(degree=%s,start=%s,end=%s,generations=%s)".format(
        degree, start, end, generations
      )
    }
  }

}
