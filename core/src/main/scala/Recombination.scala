/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                                               *
 *  Copyright  ©  2013  Christian Krause                                                         *
 *                                                                                               *
 *  Christian Krause  <kizkizzbangbang@googlemail.com>                                           *
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

import language.higherKinds

/** Provides a mechanism for recombining the genomes of individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  *
  * @define HowManyInfo How many will be returned depends solely on the implementing evolutionary
  * algorithm.
  */
trait Recombination[G,P,M[_]] {

  /** Returns new genomes by recombining the parents.
    *
    * @note $HowManyInfo
    */
  def recombine(g1: G, g2: G): M[G]

  /** Returns new genomes by recombining the parents.
    *
    * @note $HowManyInfo
    */
  final def recombine(parents: Pair[Individual[G],Individual[G]]): M[G] =
    recombine(parents._1.genome, parents._2.genome)

  def problem: P
//  def fitness(genome: G): Double
//  def Individual(genome: G): Individual[G]
}

trait Recombinator[G,P,M[_]] extends Recombination[G,P,M] {
  def evolutionary: Evolutionary[G,P]
  override final def problem: P = evolutionary.problem
//  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
//  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)
}
