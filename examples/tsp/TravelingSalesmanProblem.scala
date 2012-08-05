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
package tsp

import org.eva4s.util.graph._

import scala.annotation.tailrec

import scalax.collection._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

import scalaz._
import Scalaz._

class TravelingSalesmanProblem[N:Manifest](val problem: Graph[N,WUnDiEdge])
  extends Evolutionary[Graph[N,WDiEdge],Graph[N,WUnDiEdge]] {

  override def ancestor = cycle(problem)

  override def fitness(genome: Graph[N,WDiEdge]) = weight(genome)

  override def recombine(p1: Graph[N,WDiEdge], p2: Graph[N,WDiEdge]) = {
    val adjacencies = neighbors(p1) |+| neighbors(p2)
    val startNode = p1.nodes.head.value

    @tailrec
    def recurse(have: List[N], currentNode: N, edges: List[WDiEdge[N]]): Graph[N,WDiEdge] = {
      if (have.size == problem.nodes.size) {
        val nextEdge = (currentNode ~%> startNode)(problem.get((currentNode ~% startNode)(0)).toEdgeIn.weight)
        Graph from (
          edges = nextEdge :: edges
        )
      } else {
        val remainingNodes = adjacencies(currentNode) filterNot have.contains

        val nextNode = if (remainingNodes.nonEmpty) remainingNodes minBy { node ⇒
          adjacencies filterNot {
            case (key,_) ⇒ have.contains(key)
          } apply node filterNot have.contains size
        } else // in case remainingNodes is empty choose some remaining node
          p1.nodes.toNodeInSet filterNot have.contains head

        val nextEdge = (currentNode ~%> nextNode)(problem.get((currentNode ~% nextNode)(0)).toEdgeIn.weight)

        recurse(nextNode :: have, nextNode, nextEdge :: edges)
      }
    }

    Vector(recurse(List(startNode), startNode, Nil))
  }

  override def mutate(genome: Graph[N,WDiEdge]) = {
    val nodes = genome.nodes.toIndexedSeq
    val s = nodes.size

    val start = nodes(Random.nextInt(s))
    val end   = nodes(Random.nextInt(s))

    if (start == end) {
      genome
    } else {
      val path = start.pathTo(end).get

      if (path.edges.size == genome.edges.size - 1) {
        Graph from (
          edges = genome.edges map { e ⇒
            (e.edge._2.value ~%> e.edge._1.value)(problem.get((e.edge._2.value ~% e.edge._1.value)(0)).toEdgeIn.weight)
          }
        )
      } else {
        val pred  = start.diPredecessors.head
        val succ  = end.diSuccessors.head

        val nend   = (start.value ~%> succ.value)(problem.get((start.value ~% succ.value)(0)).toEdgeIn.weight)
        val nstart = (pred.value  ~%> end.value )(problem.get((pred.value  ~% end.value )(0)).toEdgeIn.weight)
        val nedges = path.edges map { e ⇒
          (e.edge._2.value ~%> e.edge._1.value)(problem.get((e.edge._2.value ~% e.edge._1.value)(0)).toEdgeIn.weight)
        }

        val rmes = pred.pathTo(succ) map { _.edges } getOrElse { genome.edges }

        genome -- (rmes) ++ (nstart :: nend :: nedges.toList)
      }
    }
  }

}
