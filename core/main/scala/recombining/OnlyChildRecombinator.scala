package eva4s
package recombining

import language.higherKinds

import scalaz.Id.Id

/** Factory for [[OnlyChildRecombinator]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define gene gene container
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define recombination recombination function
  * @define scaling basis for the scaling factors
  */
object OnlyChildRecombinator {

  /** Creates a new [[OnlyChildRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param f $recombination
    */
  def apply[G](f: (G,G) => G)(implicit F: Fitness[G]): OnlyChildRecombinator[G] =
    Recombinator[G,Id](f)

}
