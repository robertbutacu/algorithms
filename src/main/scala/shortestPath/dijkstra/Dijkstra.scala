package shortestPath.dijkstra

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra extends GraphExample{
  case class Node(name: String, tentativeDistance: Int = Int.MaxValue)
  case class Edge(edge: (Node, Node), distance: Int)
  type Graph = List[Edge]

  //initialising all the tentative distances with None, except for the start node


  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Option[Int] = {
    Some(0)
  }

  def removeEdge(edge: Edge, graph: Graph): Graph = {
    graph.filter(_ != edge)
  }
}
