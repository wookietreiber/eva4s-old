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


import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

package object ea {

  // -----------------------------------------------------------------------
  // aliases
  // -----------------------------------------------------------------------

  val Random = scala.util.Random

  /** A `Matchmaker` pairs individuals up with each other.
    *
    * This function models parental selection.
    */
  type Matchmaker[Individual] = Iterable[Individual] ⇒ Int ⇒ Iterable[Pair[Individual,Individual]]

  /** A `Selector` determines how the individuals for the next generation are chosen.
    *
    * This function models environmental selection.
    */
  type Selector[Individual] = Iterable[Individual] ⇒ Int ⇒ Iterable[Individual]

  // -----------------------------------------------------------------------
  // pimp my collections
  // -----------------------------------------------------------------------

  implicit def collectionExtras[A,CC[A] <: TraversableLike[A,CC[A]]](xs: CC[A]) = new {

    /** Returns a new collection with `n` randomly chosen elements. */
    def choose(n: Int)(implicit bf: CanBuildFrom[CC[A],A,CC[A]]): CC[A] =
      shuffle take n

    /** Returns a new, shuffled collection. */
    def shuffle(implicit bf: CanBuildFrom[CC[A],A,CC[A]]): CC[A] =
      Random shuffle xs

  }

}
