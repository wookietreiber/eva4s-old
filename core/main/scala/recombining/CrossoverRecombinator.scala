package eva4s
package recombining

import language.higherKinds

import scalaz.Functor
import scalaz.Length
import scalaz.Unzip
import scalaz.Zip

/** Factory for [[CrossoverRecombinator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define gene gene container
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define recombination recombination function
  * @define scaling basis for the scaling factors
  */
object CrossoverRecombinator {

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param f $recombination
    */
  def apply[G](f: (G,G) => (G,G))(implicit F: Fitness[G]): CrossoverRecombinator[G] =
    Recombinator[G,GenomeP](f)

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param underlying underlying recombinator used to create two distinct children
    */
  def biovular[G](underlying: OnlyChildRecombinator[G])(implicit F: Fitness[G]): CrossoverRecombinator[G] =
    CrossoverRecombinator { (g1: G, g2: G) =>
      val c1 = underlying.recombine(g1,g2)
      val c2 = underlying.recombine(g1,g2)

      (c1,c2)
    }

}
