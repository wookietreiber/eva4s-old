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

import scalaz.Functor

/** Provides a mechanism for recombining the genomes of individuals.
  *
  * @tparam G the type of the genome of the individuals, represents a solution of the problem
  * @tparam P input / problem type, represents the problem data structure
  * @tparam M container for the offspring; declares how many genomes/individuals are created per coupling
  */
trait Recombination[G,P,M[_]] {

  /** Returns new genomes by recombining the parents. */
  def recombine(g1: G, g2: G): M[G]

  /** Returns new genomes by recombining the parents. */
  final def recombine(parents: IndividualP[G]): M[G] =
    recombine(parents._1.genome, parents._2.genome)

  /** Returns new individuals by recombining the parents. */
  final def procreate(g1: G, g2: G)(implicit f: Functor[M]): M[Individual[G]] =
    f.map(recombine(g1,g2))(g ⇒ Individual(g))

  /** Returns new individuals by recombining the parents. */
  final def procreate(parents: IndividualP[G])(implicit f: Functor[M]): M[Individual[G]] =
    f.map(recombine(parents))(g ⇒ Individual(g))

  /** Returns the problem that needs to be solved. */
  def problem: P

  /** Returns the fitness of the given genome. */
  def fitness(genome: G): Double

  /** Returns a new individual from the given genome. */
  def Individual(genome: G): Individual[G]

}

/** Standalone [[Recombination]] building block. */
trait Recombinator[G,P,M[_]] extends Recombination[G,P,M] {

  /** Returns the evolutionary used to provide the problem and the fitness function. */
  def evolutionary: Evolutionary[G,P]

  override final def problem: P = evolutionary.problem
  override final def fitness(genome: G): Double = evolutionary.fitness(genome)
  override final def Individual(genome: G): Individual[G] = evolutionary.Individual(genome)

}
