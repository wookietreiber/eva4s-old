/* **************************************************************************
 *                                                                          *
 *  Copyright (C)  2012  Nils Foken, Christian Krause                       *
 *                                                                          *
 *  Nils Foken        <nils.foken@it2009.ba-leipzig.de>                     *
 *  Christian Krause  <christian.krause@it2009.ba-leipzig.de>               *
 *                                                                          *
 ****************************************************************************
 *                                                                          *
 *  This file is part of 'scalevalgo'.                                      *
 *                                                                          *
 *  This project is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by    *
 *  the Free Software Foundation, either version 3 of the License, or       *
 *  any later version.                                                      *
 *                                                                          *
 *  This project is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
 *  GNU General Public License for more details.                            *
 *                                                                          *
 *  You should have received a copy of the GNU General Public License       *
 *  along with this project. If not, see <http://www.gnu.org/licenses/>.    *
 *                                                                          *
 ****************************************************************************/


package ea

/** $selectioninfo */
object Selection extends Selection

/** $selectioninfo
  *
  * @define selectioninfo Contains default [[ea.Selector]] implementations which define environmental selection.
  *
  * The idiomatic usage of the functions defined here is to input the parameters of the first
  * parameter list(s) and use the remaining function as a [[ea.Selector]].
  *
  * @see [[ea.Selector]]
  *
  * @define individuals the type of the individuals
  * @define mu the amount of chosen children
  * @define parents the parents of the generation
  * @define offspring the offspring of the generation
  * @define fitness function to determine the fitness of individuals
  */
trait Selection {

  // -----------------------------------------------------------------------
  // deterministic selection
  // -----------------------------------------------------------------------

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
    * @tparam I $individuals
    *
    * @param fitness $fitness
    * @param survivors $mu, default is `μ = λ/6`
    * @param parents $parents
    * @param offspring $offspring, determines `λ = offspring.size`
    */
  def CommaSelection[I,F:Ordering](fitness: I ⇒ F,
                                   survivors: Int ⇒ Int = (λ: Int) ⇒ math.round(λ.toFloat / 6))
                                  (parents: Iterable[I], offspring: Iterable[I])
                                   : Iterable[I] =
    offspring.toSeq sortBy fitness take survivors(offspring.size)

  /** Returns the fittest individuals.
    *
    * Plus selection (aka (μ+λ) selection) represents an elitist selection, which deterministically
    * chooses the best μ individuals from all individuals (`parents ++ offspring`, i.e. `μ+λ`).
    *
    * @tparam I $individuals
    *
    * @param fitness $fitness
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring, determines `λ = offspring.size`
    */
  def PlusSelection[I,F:Ordering](fitness: I ⇒ F)
                                 (parents: Iterable[I], offspring: Iterable[I])
                                  : Iterable[I] =
    (parents ++ offspring).toSeq sortBy fitness take parents.size

  /** Returns the fittest individuals. This is an alias for [[ea.Selection#PlusSelection]].
    *
    * @tparam I $individuals
    *
    * @param fitness $fitness
    * @param parents $parents, determines `μ = parents.size`
    * @param offspring $offspring, determines `λ = offspring.size`
    *
    * @see [[ea.Selection#PlusSelection]]
    */
  def SurvivalOfTheFittest[I,F:Ordering](fitness: I ⇒ F)
                                        (parents: Iterable[I], offspring: Iterable[I])
                                         : Iterable[I] =
    PlusSelection(fitness)(parents,offspring)

  // -----------------------------------------------------------------------
  // probabilistic selection
  // -----------------------------------------------------------------------

  /** Returns individuals arbitrarily from both `parents` and `offspring`. This is the simplest form
    * of probabilistic selection.
    *
    * @tparam I $individuals
    *
    * @param survivors $mu
    * @param parents $parents
    * @param offspring $offspring
    */
  def RandomSelection[I](survivors: Int)
                        (parents: Iterable[I], offspring: Iterable[I])
                         : Iterable[I] =
    (parents ++ offspring) choose survivors

}
