package eva4s

import scala.util.Random

package object recombining {

  private[recombining] def sample(x: Double): Double =
    nextDoubleWithin(-x, 1 + x)

  private[recombining] def nextDoubleWithin(lower: Double, upper: Double): Double =
    lower + (upper - lower) * Random.nextDouble

}
