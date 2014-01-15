package org.eva4s
package selecting

import scalay.collection._

/** Pure probabilistic selection. */
object RandomSelection extends DocDummy {

  /** Returns individuals arbitrarily from both `parents` and `offspring`. This is the simplest form
    * of probabilistic selection.
    *
    * @tparam G $genome
    *
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring
    */
  def apply[G](parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    (parents ++ offspring) choose parents.size

}
