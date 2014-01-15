package org.eva4s
package selecting

/** Deterministic selection. */
object ChildSelection extends DocDummy {

  /** Returns the offspring. This is a simple deterministic `Selector` that chooses all children and
    * lets all parents die.
    *
    * @tparam G $genome
    *
    * @param parents $parents
    * @param offspring $offspring
    */
  def apply[G](parents: Seq[Individual[G]], offspring: Seq[Individual[G]]): Seq[Individual[G]] =
    offspring

}
