/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2012  Nils Foken, Christian Krause                                             *
 *                                                                                               *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                                          *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>                                    *
 *                    <kizkizzbangbang@googlemail.com>                                           *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  This file is part of 'eva4s'.                                                                *
 *                                                                                               *
 *  This project is free software: you can redistribute it and/or modify it under the terms      *
 *  of the GNU General Public License as published by the Free Software Foundation, either       *
 *  version 3 of the License, or any later version.                                              *
 *                                                                                               *
 *  This project is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;    *
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    *
 *  See the GNU General Public License for more details.                                         *
 *                                                                                               *
 *  You should have received a copy of the GNU General Public License along with this project.   *
 *  If not, see <http://www.gnu.org/licenses/>.                                                  *
 *                                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


package org.eva4s

import scalay.collection._

/** $SelectionInfo */
object Selection extends Selection

/** $SelectionInfo
  *
  * @define SelectionInfo Contains default [[org.eva4s.Selector]] implementations which define
  * environmental selection.
  *
  * The idiomatic usage of the functions defined here is to fill in the parameters of the first
  * parameter list(s) and use the remaining function as a [[org.eva4s.Selector]].
  *
  * @see [[org.eva4s.Selector]]
  *
  * @define genome the type of the genome of the individuals
  * @define mu the amount of chosen children
  * @define parents the parents of the generation
  * @define offspring the offspring of the generation
  */
trait Selection {

  // -----------------------------------------------------------------------
  // deterministic selection
  // -----------------------------------------------------------------------

  /** Returns the offspring. This is a simple deterministic `Selector` that chooses all children and
    * lets all parents die.
    *
    * @tparam G $genome
    *
    * @param parents $parents
    * @param offspring $offspring
    */
  def ChildSelection[G](parents: Seq[Individual[G]], offspring: Seq[Individual[G]])
                        : Seq[Individual[G]] = offspring

  /** Returns the fittest offspring.
    *
    * Comma selection (aka (μ,λ) selection) represents an elitist selection, which deterministically
    * chooses the best μ individuals (`survivors`) from the offspring of size λ. As smaller as the
    * `μ/λ` ratio gets, the higher is the ''selection pressure'' (aka ''evolutionary pressure'').
    * Recommended is to choose `μ` in a way so that `1/7 ≤ μ/λ ≤ 1/5`, which is done by default.
    *
    * '''Note:''' The parents (the survivors of the previous generation) are not considered with
    * this kind of selection.
    *
    * @tparam G $genome
    *
    * @param survivors $mu, default is `μ = λ/6`
    * @param parents $parents
    * @param offspring $offspring, determines `λ = offspring.size`
    */
  def CommaSelection[G](survivors: Int ⇒ Int = (λ: Int) ⇒ math.round(λ.toFloat / 6))
                       (parents: Seq[Individual[G]], offspring: Seq[Individual[G]])
                        : Seq[Individual[G]] =
    offspring sortBy { _.fitness } take survivors(offspring.size)

  /** Returns the fittest individuals.
    *
    * Plus selection (aka (μ+λ) selection) represents an elitist selection, which deterministically
    * chooses the best μ individuals from all individuals (`parents ++ offspring`, i.e. `μ+λ`).
    *
    * @tparam G $genome
    *
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring, determines `λ = offspring.size`
    */
  def PlusSelection[G](parents: Seq[Individual[G]],
                       offspring: Seq[Individual[G]])
                       : Seq[Individual[G]] =
    (parents ++ offspring) sortBy { _.fitness } take parents.size

  /** Returns the fittest individuals. This is an alias for [[org.eva4s.Selection#PlusSelection]].
    *
    * @tparam G $genome
    *
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring, determines `λ = offspring.size`
    *
    * @see [[org.eva4s.Selection#PlusSelection]]
    */
  def SurvivalOfTheFittest[G](parents: Seq[Individual[G]],
                              offspring: Seq[Individual[G]])
                              : Seq[Individual[G]] =
    PlusSelection(parents,offspring)

  // -----------------------------------------------------------------------
  // probabilistic selection
  // -----------------------------------------------------------------------

  /** Returns individuals arbitrarily from both `parents` and `offspring`. This is the simplest form
    * of probabilistic selection.
    *
    * @tparam G $genome
    *
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring
    */
  def RandomSelection[G](parents: Seq[Individual[G]], offspring: Seq[Individual[G]])
                         : Seq[Individual[G]] =
    (parents ++ offspring) choose parents.size

}
