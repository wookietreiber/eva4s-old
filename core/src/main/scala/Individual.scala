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

/** Represents a candidate solution of a problem and its fitness.
  *
  * == Genome ==
  *
  * Individuals carry genetic information, the [[Individual!.genome genome]]. The genome directly
  * represents a candidate solution to a [[Evolutionary!.problem problem]].
  *
  * == Fitness ==
  *
  * The [[Individual!.fitness fitness]] of an individual, in biology, describes its ability to both
  * survive and reproduce. In the context of evolutionary algorithms it serves as a value describing
  * how optimal a candidate solution is for solving a given [[Evolutionary!.problem problem]]. The
  * fitness of an individual is used by both [[Selection environmental selection]] and
  * [[Matchmaking parental selection]].
  *
  * @tparam G the type of the genome of the individuals
  *
  * @param genome Returns the genome of this individual.
  * @param fitness Returns the fitness of this individual.
  *
  * @todo separation of genotype and phenotype
  * @todo generic fitness type ([[scala.math.Numeric Numeric]], [[scala.math.Ordering Ordering]])
  */
case class Individual[G](genome: G, fitness: Double)
