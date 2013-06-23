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

/** Factory for [[OnlyChildRecombination]] instances.
  *
  * @define genome the type of the genome of the individuals, represents a solution of the problem
  * @define problem input / problem type, represents the problem data structure
  * @define evolutionary evolutionary providing the problem and the fitness function
  * @define recombination recombination function
  */
object OnlyChildRecombinator {

  /** Creates a new [[OnlyChildRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $recombination, depending on the problem
    */
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ (G,G) ⇒ G): OnlyChildRecombinator[G,P] = new OnlyChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): G = f(evolutionary.problem)(g1,g2)
  }

  /** Creates a new [[OnlyChildRecombinator]].
    *
    * @tparam G $genome
    * @tparam P $problem
    *
    * @param e $evolutionary
    * @param f $recombination
    */
  def independent[G,P](ep: Evolutionary[G,P])(f: (G,G) ⇒ G): OnlyChildRecombinator[G,P] = new OnlyChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): G = f(g1,g2)
  }

}
