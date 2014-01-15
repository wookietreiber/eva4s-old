package org.eva4s
package recombining

import language.higherKinds

import scalaz.Functor
import scalaz.Length
import scalaz.Unzip
import scalaz.Zip

/** Standalone [[Recombinator]] building block which produces two children per recombination. */
trait CrossoverRecombinator[G,P] extends Recombinator[G,P,GenomeP]

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
    * @param e $evolutionary
    * @param f $recombination, depending on the problem
    */
  def apply[G,P](e: Evolutionary[G,P])(f: P ⇒ (G,G) ⇒ (G,G)): CrossoverRecombinator[G,P] = new CrossoverRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def recombine(g1: G, g2: G): (G,G) = f(evolutionary.problem)(g1,g2)
  }

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $recombination
    */
  def independent[G,P](e: Evolutionary[G,P])(f: (G,G) ⇒ (G,G)): CrossoverRecombinator[G,P] = new CrossoverRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = e
    override def recombine(g1: G, g2: G): (G,G) = f(g1,g2)
  }

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param recombinator recombinator to use to create two distinct children
    */
  def biovular[G,P](recombinator: OnlyChildRecombinator[G,P]): CrossoverRecombinator[G,P] = new CrossoverRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = recombinator.evolutionary
    override def recombine(g1: G, g2: G): (G,G) = {
      def child = recombinator.recombine(g1,g2)
      (child,child)
    }
  }

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam F $gene
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param scaling $scaling
    */
  def arithmetic[F[_],P](e: Evolutionary[F[Double],P])(scaling: Double = ArithmeticCrossover.defaultScaling)(implicit F: Functor[F], Z: Zip[F]): CrossoverRecombinator[F[Double],P] =
    independent(e)(ArithmeticCrossover.recombine(scaling))

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam F $gene
    * @tparam P $problem
    *
    * @param e $evolutionary
    */
  def onePoint[F[_] : Functor : Length : Unzip : Zip,A,P](e: Evolutionary[F[A],P]): CrossoverRecombinator[F[A],P] =
    independent(e)(OnePointCrossover.recombine)

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam F $gene
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param mixingRatio determines gene distribution between the children
    */
  def uniform[F[_] : Functor : Unzip : Zip,A,P](e: Evolutionary[F[A],P])(mixingRatio: Double = UniformCrossover.defaultMixingRatio): CrossoverRecombinator[F[A],P] =
    independent(e)(UniformCrossover.recombine(mixingRatio))

  /** Creates a new [[CrossoverRecombinator]].
    *
    * @tparam F $gene
    * @tparam P $problem
    *
    * @param e $evolutionary
    */
  def twoPoint[F[_] : Functor : Length : Unzip : Zip,A,P](e: Evolutionary[F[A],P]): CrossoverRecombinator[F[A],P] =
    independent(e)(TwoPointCrossover.recombine)

}
