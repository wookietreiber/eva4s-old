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

import scalaz.Id.Id

/** Recombination that per parent pair produces only one child. */
trait OnlyChildRecombination[G,P] extends Recombination[G,P,Id] {
}

trait OnlyChildRecombinator[G,P] extends Recombinator[G,P,Id] {
}

object OnlyChildRecombinator {
  def apply[G,P](ep: Evolutionary[G,P])(f: P ⇒ (G,G) ⇒ G) = new OnlyChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): G = f(evolutionary.problem)(g1,g2)
  }

  def independent[G,P](ep: Evolutionary[G,P])(f: (G,G) ⇒ G) = new OnlyChildRecombinator[G,P] {
    override val evolutionary: Evolutionary[G,P] = ep
    override def recombine(g1: G, g2: G): G = f(g1,g2)
  }
}
