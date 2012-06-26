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
package util

import scalax.collection._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

object graph {

  /** Returns a complete, undirected graph. */
  def completeWUnDiGraph[N](nodes: Iterable[N])(f: (N,N) ⇒ Long): Graph[N,WUnDiEdge] = Graph from (
    edges = for {
      a ← nodes
      b ← nodes if a != b
      w = f(a,b)
    } yield (a ~% b)(w)
  )

  /** Returns a complete, undirected graph with random weights. */
  def completeWUnDiGraph[N](nodes: Iterable[N], maxWeight: Int): Graph[N,WUnDiEdge] =
    completeWUnDiGraph(nodes) { (_,_) ⇒
      Random.nextInt(maxWeight) + 1
    }

  /** Returns the total weight of the given graph. */
  def weight[N,E[X] <: EdgeLikeIn[X]](g: Graph[N,E]): Long =
    g.edges.foldLeft(0L) { _ + _.weight }

  /** Returns the neighbors of all nodes of the given graph. */
  def neighbors[N,E[X] <: EdgeLikeIn[X]](g: Graph[N,E]): Map[N,collection.Set[N]] = g.nodes map { node ⇒
    node.value → (node.neighbors map { _.value })
  } toMap

  /** Returns a random hamiltonian cycle of the given complete graph. */
  def cycle[N](g: Graph[N,WUnDiEdge]): Graph[N,WDiEdge] = {
    val nodes = g.nodes.toNodeInSet.toIndexedSeq.shuffle

    Graph from (
      edges = for {
        i ← 0 until nodes.size
        j = (i+1) % nodes.size
        s = nodes(i)
        e = nodes(j)
        w = g get ((s ~% e)(0)) weight
      } yield (s ~%> e)(w)
    )
  }

}
