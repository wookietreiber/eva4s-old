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

/** Provides the means to recombine the genomes of individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  *
  * @define HowManyInfo How many will be returned depends solely on the implementing evolutionary
  * algorithm.
  */
trait Recombination[G,P] {

  self: Evolutionary[G,P] ⇒

  /** Returns new genomes by recombining the parents.
    *
    * @note $HowManyInfo
    */
  def recombine(p1: G, p2: G): Seq[G]

  /** Returns new genomes by recombining the parents.
    *
    * @note $HowManyInfo
    */
  final def recombine(parents: Pair[Individual[G],Individual[G]]): Seq[G] =
    recombine(parents._1.genome, parents._2.genome)

  /** Returns new individuals by recombining the parents.
    *
    * @note $HowManyInfo
    */
  final def procreate(parents: Pair[Individual[G],Individual[G]]): Seq[Individual[G]] = for {
    genome ← recombine(parents)
  } yield Individual(genome)

}
