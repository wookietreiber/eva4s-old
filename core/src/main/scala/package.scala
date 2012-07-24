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


import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom

import scalax.util._

/**
  *
  * @define genome the type of the genome of the individuals
  */
package object ea {

  /** Represents an individual with its genome and fitness.
    *
    * @tparam G the type of the genome of the individuals
    *
    * @param genome Returns the genome of this individual.
    * @param fitness Returns the fitness of this individual.
    */
  case class Individual[G](genome: G, fitness: Double)

  // -----------------------------------------------------------------------
  // aliases
  // -----------------------------------------------------------------------

  val Random = scala.util.Random

  /** A `Matchmaker` pairs individuals up with each other.
    *
    * @tparam G $genome
    *
    * @see [[ea.Matchmaking]]
    */
  type Matchmaker[G] = Iterable[Individual[G]] ⇒ Iterable[Pair[Individual[G],Individual[G]]]

  /** A `Mutagen` determines the probability with which individuals mutate, depending on the current
    * generation.
    *
    * @see [[ea.Mutagens]]
    */
  type Mutagen = Int ⇒ Double

  /** A `Selector` determines how the individuals for the next generation are chosen.
    *
    * @tparam G $genome
    *
    * @see [[ea.Selection]]
    */
  type Selector[G] = (Iterable[Individual[G]], Iterable[Individual[G]]) ⇒ Iterable[Individual[G]]

  // -----------------------------------------------------------------------
  // pimp my collections
  // -----------------------------------------------------------------------

  implicit def genCollectionExtras[A,CC[A] <: GenTraversable[A]](xs: CC[A]) = new {

    /** Returns the average of the elements in this collection. */
    def average(implicit num: Numeric[A]): Double = {
      import num._
      xs.sum.toDouble / xs.size
    }

    /** Returns the average of the results of the applied function.
      *
      * `coll averageBy f` is equivalent to `coll map f average` but does not entail the overhead
      * of creating a new collection.
      */
    def averageBy[B](f: A ⇒ B)(implicit num: Numeric[B]): Double = {
      import num._
      xs.aggregate(zero)(_ + f(_), _ + _).toDouble / xs.size
    }

    /** Returns a new collection with `n` randomly chosen elements. */
    def choose(n: Int): CC[A] =
      xs.shuffle(Mixer.GenTraversableMixer[CC]).take(n).asInstanceOf[CC[A]]

    /** Returns two randomly chosen elements as a pair. */
    def choosePair: Pair[A,A] = {
      val two = choose(2).seq
      Pair(two.head, two.last)
    }

  }

  /** Returns some debugger function. */
  val printer: Option[(Int,Double,Double) ⇒ Unit] = Some { (g: Int, i: Double, f: Double) ⇒
    printf("generation: %5d   intensity: % 10.6f   average fitness: %f\n", g, i, f)
  }

  private[ea] def ranks(size: Int) = Vector.tabulate(size) {
    i ⇒ 2.0 / size * (1 - i.toDouble / (size - 1.0))
  }

}
