package org.eva4s
package mutating

object ExponentialMutagen extends DocDummy {

  /** Returns a monotonic function based on `f(x) = a * exp(b*x)`.
    *
    * @param start $start
    * @param end $end
    * @param generations $generations
    */
  def apply(generations: Int, start: Double = 0.8, end: Double = 0.01): Mutagen = new Function[Int,Double] {
    val a = start
    val c = end / start

    override def apply(generation: Int): Double = a * math.pow(c, generation.toDouble / generations)

    override def toString = "ExponentialMutagen(start=%s,end=%s,generations=%s)".format(
      start, end, generations
    )
  }

}
