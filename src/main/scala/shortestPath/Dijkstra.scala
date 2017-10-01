package shortestPath

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra {
  case class Distance(dist: Int)
  case class TentativeDistance(from: Node, to: Node, dist: Option[Int] = None)
  case class Node(name: String, neighbours: List[Map[Node, Distance]])
  case class Graph(nodes: Set[Node])

  def initialize(start: Node, graph: Graph): List[TentativeDistance] = {
    List()
  }

  def shortest(curr: Node,
               goalNode: Node,
               tentDist: List[TentativeDistance],
               graph: Graph,
               visited: Set[Node],
               path: List[Node]): List[Node] = {
    Nil
  }

  def removeTentativeNode(replacement: TentativeDistance, tentNodes: List[TentativeDistance]): List[TentativeDistance] = {
    tentNodes filter { node => node.to == replacement.to && node.from == replacement.from }
  }

  def addTentativeNode(newNode: TentativeDistance, tentNodes: List[TentativeDistance]): List[TentativeDistance] = {
    tentNodes :+ newNode
  }

}
