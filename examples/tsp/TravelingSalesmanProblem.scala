package tsp

import util.graph._

import scala.annotation.tailrec
import scala.util.Random

import scalax.collection._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

import scalaz._
import Scalaz._

trait TravelingSalesmanProblem[N] {

  type G[N] = Graph[N,WDiEdge]
  type P[N] = Graph[N,WUnDiEdge]

  implicit def manifest: Manifest[N]

  def problem: P[N]

  def fitness(genome: G[N]): Double =
    weight(genome)

  def create: G[N] =
    cycle(problem)

  def mutate(genome: G[N]): G[N] = {
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
            (e.edge._2.value ~%> e.edge._1.value)(problem.get((e.edge._2.value ~% e.edge._1.value)(0)).toOuter.weight)
          }
        )
      } else {
        val pred  = start.diPredecessors.head
        val succ  = end.diSuccessors.head

        val nend   = (start.value ~%> succ.value)(problem.get((start.value ~% succ.value)(0)).toOuter.weight)
        val nstart = (pred.value  ~%> end.value )(problem.get((pred.value  ~% end.value )(0)).toOuter.weight)
        val nedges = path.edges map { e ⇒
          (e.edge._2.value ~%> e.edge._1.value)(problem.get((e.edge._2.value ~% e.edge._1.value)(0)).toOuter.weight)
        }

        val rmes = pred.pathTo(succ) map { _.edges } getOrElse { genome.edges }

        genome -- (rmes) ++ (nstart :: nend :: nedges.toList)
      }
    }
  }

  def recombine(g1: G[N], g2: G[N]): G[N] = {
    val adjacencies = neighbors(g1) |+| neighbors(g2)
    val startNode = g1.nodes.head.value

    @tailrec
    def recurse(have: List[N], currentNode: N, edges: List[WDiEdge[N]]): G[N] = {
      if (have.size == problem.nodes.size) {
        val nextEdge = (currentNode ~%> startNode)(problem.get((currentNode ~% startNode)(0)).toOuter.weight)
        Graph from (
          edges = nextEdge :: edges
        )
      } else {
        val remainingNodes = adjacencies(currentNode) filterNot have.contains

        val nextNode = if (remainingNodes.nonEmpty) remainingNodes minBy { node ⇒
          adjacencies.filterNot({
            case (key,_) ⇒ have.contains(key)
          }).apply(node).filterNot(have.contains).size
        } else // in case remainingNodes is empty choose some remaining node
          g1.nodes.toOuter.filterNot(have.contains).head

        val nextEdge = (currentNode ~%> nextNode)(problem.get((currentNode ~% nextNode)(0)).toOuter.weight)

        recurse(nextNode :: have, nextNode, nextEdge :: edges)
      }
    }

    recurse(List(startNode), startNode, Nil)
  }

}
