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

/** Contains default `Selector` implementations.
  *
  * A `Selector` models environmental selection.
  */
object Selectors {

  /** Returns an arbitrarily choosing selector.
    *
    * This is the simplest form of probabilistic selection.
    */
  def Random[Individual]: Selector[Individual] =
    (individuals: Iterable[Individual]) ⇒ (survivors: Int) ⇒
      individuals choose survivors

  /** Returns a selector that chooses only the fittest individuals.
    *
    * This is also known as deterministic selection.
    */
  def SurvivalOfTheFittest[Individual](fitness: Individual ⇒ Double): Selector[Individual] =
    (individuals: Iterable[Individual]) ⇒ (survivors: Int) ⇒
      individuals.toSeq sortBy fitness take survivors

}
