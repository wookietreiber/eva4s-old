package eva4s
package reporting

import eva4s.util._

trait Deviation {

  self: Reporter =>

  def deviation(nextGen: Seq[Individual[_]]): (Double,Double,Double) = {
    val fittest = nextGen.minBy(_.fitness).fitness
    val unfittest = nextGen.maxBy(_.fitness).fitness
    val average = nextGen.geometricMeanBy(_.fitness)

    (fittest, average, unfittest)
  }

}
