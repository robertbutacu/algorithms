package shortestPath

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra {
  case class Distance(dist: Int)
  case class TentativeDistance(node: Node, dist: Option[Int] = None)
  case class Node(name: String, neighbours: Option[Map[Node, Distance]] = None)
  case class Graph(nodes: Set[Node])

  def initialize(start: Node, graph: Graph): Set[TentativeDistance] = {
    graph.nodes map {node: Node => if(node == start) TentativeDistance(node, Some(0)) else TentativeDistance(node, None)}
  }

  //@tailrec
  def shortest(curr: Node,
               goalNode: Node,
               tentDist: Set[TentativeDistance],
               graph: Graph,
               visited: Set[Node],
               path: Set[Node]): Set[Node] = {
    Set()
  }

  def removeTentativeNode(replacement: TentativeDistance, tentNodes: Set[TentativeDistance]): Set[TentativeDistance] = {
    tentNodes filter { _.node == replacement.node }
  }

  def addTentativeNode(newNode: TentativeDistance, tentNodes: Set[TentativeDistance]): Set[TentativeDistance] = {
    tentNodes + newNode
  }

}
