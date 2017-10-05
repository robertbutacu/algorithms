package shortestPath.utils

import shortestPath.dijkstra.Dijkstra.Graph

trait Utils {
  type Graph = List[Node]
  type Distance = Int
  type Edges = List[(Node, Distance)]
  type Path = List[Node]

  def initialize(start: Node, graph: Graph): Unit = {
    graph foreach { node =>
      if (node == start)
        node.tentativeDistance = 0
      else
        node.tentativeDistance = Int.MaxValue
      node.previous = None
    }
  }

  def pathDijkstra(start: Node): List[Node] = {
    start.previous match {
      case Some(node) => pathDijkstra(node) ::: List(start)
      case None       => List(start)
    }
  }

  def pathBellmanFord(end: Node, start: Node): List[Node] = {
    if (end == start)
      List(end)
    else{
      end.previous match {
        case Some(node) => pathBellmanFord(node, start) ::: List(end)
        case None       => List(end)
      }
    }
  }
}
