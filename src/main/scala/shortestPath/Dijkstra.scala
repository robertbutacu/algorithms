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

  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Set[Node] = {
    //@tailrec
    def go(curr: Node, goalNode: Node, tentDist: Set[TentativeDistance], visited: Set[Node], path: Set[Node]): Set[Node] = {
      Set()
    }

    go(
      start,
      goalNode,
      Set.empty[TentativeDistance],
      Set.empty[Node],
      Set.empty[Node]
    )
  }

  def removeTentativeNode(replacement: TentativeDistance, tentNodes: Set[TentativeDistance]): Set[TentativeDistance] = {
    tentNodes filter { _.node == replacement.node }
  }

  def addTentativeNode(newNode: TentativeDistance, tentNodes: Set[TentativeDistance]): Set[TentativeDistance] = {
    tentNodes + newNode
  }

}
