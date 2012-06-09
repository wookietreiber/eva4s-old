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

import ea.util.graph._

import scalax.collection._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

import scalaz._
import Scalaz._

object tsp {

  def apply[N](g: Graph[N,WUnDiEdge])
              (psize: Int = 10, generations: Int = 2000, csize: Int = 40, cprop: Double = 0.3) = {

    def evolve(oldGen: Seq[Graph[N,WDiEdge]], generations: Int): Graph[N,WDiEdge] = generations match {
      case 0 ⇒ oldGen minBy weight

      case _ ⇒
        val children = for {
          i ← 1 to csize if Random.nextDouble < cprop
          parents = choose(2)(oldGen)
          child   = findChild(g, parents)
        } yield mutate(g, child)

        val nextGen = (oldGen ++ children) sortBy weight take psize

        evolve(nextGen, generations - 1)
    }

    evolve(initialPopulation(g, psize), generations)
  }

  def initialPopulation[N](g: Graph[N,WUnDiEdge], psize: Int): Seq[Graph[N,WDiEdge]] =
    for (i ← 1 to psize) yield cycle(g)

  def mutate[N](g: Graph[N,WUnDiEdge], c: Graph[N,WDiEdge]): Graph[N,WDiEdge] = {
    val nodes = choose(2)(c.nodes)

    val start = nodes.head
    val end   = nodes.last

    if (start == end)
      return c

    val path = start.pathTo(end).get

    if (path.edges.size == c.edges.size - 1) return Graph.from (
      c.nodes.toNodeInSet,
      c.edges map { e ⇒
        e.edge._2.value ~> e.edge._1.value % (g.get(e.edge._2.value ~ e.edge._1.value % 0).weight)
      }
    )

    val pred  = start.diPredecessors.head
    val succ  = end.diSuccessors.head

    val nend   = start.value ~> succ.value % (g.get(start.value ~ succ.value % 0).weight)
    val nstart = pred.value  ~> end.value  % (g.get(pred.value  ~ end.value  % 0).weight)
    val nedges = path.edges map { e ⇒
      e.edge._2.value ~> e.edge._1.value % (g.get(e.edge._2.value ~ e.edge._1.value % 0).weight)
    }

    val rmes = pred.pathTo(succ) map { _.edges } getOrElse { c.edges }

    c -- (rmes) ++ (nstart :: nend :: nedges.toList)
  }

  def findChild[N](g: Graph[N,WUnDiEdge], ps: Iterable[Graph[N,WDiEdge]]): Graph[N,WDiEdge] = {
    val adj = ps.map(neighbors).fold(Map()) { _ |+| _ }

    val startNode: N = ps.head.nodes.head.value

    def recurse(have: List[N], currentNode: N, child: Graph[N,WDiEdge]): Graph[N,WDiEdge] = {
      if (have.size == g.nodes.size) {
        child + (currentNode ~> startNode % (g.get(currentNode ~ startNode % 0).weight))
      } else {
        val set = adj(currentNode) filterNot have.contains

        val filtered = adj filterNot { case (k,_) ⇒ have.contains(k) }

        val nextNode = set minBy { node ⇒ filtered(node) filterNot have.contains size }

        val nextEdge = currentNode ~> nextNode % (g.get(currentNode ~ nextNode % 0).weight)

        recurse(nextNode :: have, nextNode, child + nextEdge)
      }
    }

    recurse(List(startNode), startNode, Graph.from(g.nodes.toNodeInSet,Nil))
  }

}
