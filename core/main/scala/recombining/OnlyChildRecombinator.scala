package org.eva4s
package recombining

import language.higherKinds

import scalaz.Id.Id
import scalaz.Functor
import scalaz.Zip

/** Standalone [[Recombinator]] building block that produces a single offspring per recombination. */
trait OnlyChildRecombinator[G,P] extends Recombinator[G,P,Id]

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
    * @param e $evolutionary
    * @param f $recombination, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ (G,G) ⇒ G): OnlyChildRecombinator[G,P] = new OnlyChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def recombine(g1: G, g2: G): G = f(evolutionary.problem)(g1,g2)
  }

  /** Creates a new [[OnlyChildRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $recombination
    */
  def independent[G,P](e: Evolutionary[G,P])(f: (G,G) ⇒ G): OnlyChildRecombinator[G,P] = new OnlyChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def recombine(g1: G, g2: G): G = f(g1,g2)
  }

  /** Creates a new [[OnlyChildRecombinator]].
    *
    * @tparam F $gene
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param scaling $scaling
    */
  def intermediate[F[_],P](e: Evolutionary[F[Double],P])(scaling: Double = IntermediateRecombinator.defaultScaling)(implicit F: Functor[F], Z: Zip[F]): OnlyChildRecombinator[F[Double],P] =
    independent(e)(IntermediateRecombinator.recombine(scaling))

  /** Creates a new [[OnlyChildRecombinator]].
    *
    * @tparam F $gene
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param scaling $scaling
    */
  def line[F[_],P](e: Evolutionary[F[Double],P])(scaling: Double = LineRecombinator.defaultScaling)(implicit F: Functor[F], Z: Zip[F]): OnlyChildRecombinator[F[Double],P] =
    independent(e)(LineRecombinator.recombine(scaling))

}
