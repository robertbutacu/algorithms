package shortestPath.utils

import shortestPath.dijkstra.imperative.Dijkstra.Graph

trait Utils {
  type Graph = List[Node]
  type Distance = Int
  type Edges = List[(Node, Distance)]
  type Path = (Graph, Distance)

  def initialize(start: Node, graph: Graph): Unit = {
    graph foreach { node =>
      if (node == start)
        node.tentativeDistance = 0
      else
        node.tentativeDistance = Int.MaxValue
      node.previous = None
    }
  }

  def path(start: Node): List[Node] = {
    start.previous match {
      case Some(node) => path(node) ::: List(start)
      case None => List(start)
    }
  }
}
