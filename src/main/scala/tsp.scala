/* **************************************************************************
 *                                                                          *
 *  Copyright (C)  2012  Nils Foken, Christian Krause                       *
 *                                                                          *
 *  Nils Foken       <nils.foken@it2009.ba-leipzig.de>                      *
 *  Christian Krause <christian.krause@it2009.ba-leipzig.de>                *
 *                                                                          *
 ****************************************************************************
 *                                                                          *
 *  This file is part of 'ea'.                                              *
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

import scala.util.Random._

import scalax.collection._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

import scalaz._
import Scalaz._

object tsp {

  def rndWUnDiComplete(n: Int, maxW: Int): Graph[Int,WUnDiEdge] = {
    val edges = for {
      i ← (  1) to (n-1)
      j ← (i+1) to (n  )
      w = nextInt(maxW) + 1
    } yield i ~ j % w

    Graph.from(1 to n, edges)
  }

  def apply[N](g: Graph[N,WUnDiEdge])
              (populationSize: Int = 10, generations: Int = 2000, rsize: Int = 40, rprop: Double = 0.3) = {

    def evolve(population: IndexedSeq[Graph[N,WDiEdge]], generations: Int): (Graph[N,WDiEdge],Long) = generations match {
      case 0 ⇒ population map { individual ⇒
        individual → weight(individual)
      } minBy { _._2 }

      case _ ⇒
        val youngsters = for {
          i ← 1 to rsize if nextDouble < rprop
          parents = choose(population)(2)
          child   = findChild(g, parents)
        } yield child

        val newPopulus = (population ++ youngsters) map { individual ⇒
          individual → weight(individual)
        } sortBy {
          _._2
        } take 10

        evolve(newPopulus map { _._1 }, generations - 1)
    }

    evolve(for (i ← 1 to populationSize) yield circle(g), generations)
  }

  def mutate[N](g: Graph[N,WUnDiEdge], c: Graph[N,WDiEdge]): Graph[N,WDiEdge] = {
    val nodes = choose(c.nodes.toIndexedSeq)(2)

    val start = nodes.head
    val end   = nodes.last

    printf("start: %s   end: %s\n", start, end)

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

    printf("start: %s   end: %s\n", start, end)

    val nend   = start.value ~> succ.value % (g.get(start.value ~ succ.value % 0).weight)
    val nstart = pred.value  ~> end.value  % (g.get(pred.value  ~ end.value  % 0).weight)
    val nedges = path.edges map { e ⇒
      e.edge._2.value ~> e.edge._1.value % (g.get(e.edge._2.value ~ e.edge._1.value % 0).weight)
    }

    val rmes = pred.pathTo(succ) map { _.edges } getOrElse { c.edges }

    c -- (rmes) ++ (nstart :: nend :: nedges.toList)
  }

  def findChild[N](g: Graph[N,WUnDiEdge], ps: IndexedSeq[Graph[N,WDiEdge]]) = {
    val adj = ps.map(neighbors(_)).fold(Map())(_ |+| _)

    val startNode: N = ps.head.nodes.head.value

    def recurse(have: List[N], currentNode: N, child: Graph[N,WDiEdge]): Graph[N,WDiEdge] = {
      if (have.size == g.nodes.size) {
        child + (currentNode ~> startNode % (g.get(currentNode ~ startNode % 0).weight))
      } else {
        val set = adj(currentNode)
        val filtered = adj filterNot { case (k,_) ⇒ have.contains(k) }

        val nextNode = set filterNot have.contains map { node ⇒
          node → ( filtered(node) filterNot have.contains )
        } minBy { _._2.size } _1

        val nextEdge = currentNode ~> nextNode % (g.get(currentNode ~ nextNode % 0).weight)

        recurse(nextNode :: have, nextNode, child + nextEdge)
      }
    }

    val child = recurse(List(startNode), startNode, Graph.from(g.nodes.toNodeInSet,Nil))

    child
  }

  def weight(g: Graph[_,WDiEdge]): Long =
    g.edges.toList map { _.weight } sum

  def circle[N](g: Graph[N,WUnDiEdge]): Graph[N,WDiEdge] = {
    val ns = shuffle(g.nodes.toNodeInSet.toIndexedSeq)

    val cycle = for {
      i ← 0 until ns.size
      j = (i+1) % ns.size
      s = ns(i)
      e = ns(j)
      w = g.get(s ~ e % 0).weight
    } yield (s ~%> e)(w)

    Graph.from(g.nodes.toNodeInSet, cycle)
  }

  def choose[A](as: IndexedSeq[A])(n: Int = 2): IndexedSeq[A] = {
    val s = as.size

    for (i ← 1 to n) yield as(nextInt(s-1))
  }

  def neighbors[N,E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) = g.nodes map { node ⇒
    node.value → (node.neighbors map { _.value })
  } toMap

}
