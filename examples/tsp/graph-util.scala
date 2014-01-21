package tsp
package util

import language.postfixOps
import language.higherKinds

import scala.util.Random

import scalax.collection._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

import eva4s.util._

object graph {

  /** Returns a complete, undirected graph. */
  def completeWUnDiGraph[N: Manifest](nodes: Iterable[N])(f: (N,N) ⇒ Long): Graph[N,WUnDiEdge] = Graph from (
    edges = for {
      a ← nodes
      b ← nodes if a != b
      w = f(a,b)
    } yield (a ~% b)(w)
  )

  /** Returns a complete, undirected graph with random weights. */
  def completeWUnDiGraph[N: Manifest](nodes: Iterable[N], maxWeight: Int): Graph[N,WUnDiEdge] =
    completeWUnDiGraph(nodes) { (_,_) ⇒
      Random.nextInt(maxWeight) + 1
    }

  /** Returns the total weight of the given graph. */
  def weight[N,E[X] <: EdgeLikeIn[X]](g: Graph[N,E]): Long =
    g.edges.foldLeft(0L) { _ + _.weight }

  /** Returns the neighbors of all nodes of the given graph. */
  def neighbors[N,E[X] <: EdgeLikeIn[X]](g: Graph[N,E]): Map[N,Set[N]] = g.nodes map { node ⇒
    node.value → node.neighbors.map(_.value).toSet
  } toMap

  /** Returns a random hamiltonian cycle of the given complete graph. */
  def cycle[N: Manifest](g: Graph[N,WUnDiEdge]): Graph[N,WDiEdge] = {
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
