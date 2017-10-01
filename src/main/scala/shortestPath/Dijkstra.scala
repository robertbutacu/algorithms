package shortestPath

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra {
  case class Distance(dist: Int)
  case class TentativeDistance(node: Node, dist: Option[Int] = None)
  case class Node(name: String)
  case class Edge(from: Node, to: Node)
  case class Graph(nodes: Map[Edge, Distance])

  def initialize(start: Node, graph: Graph): Set[TentativeDistance] = {
    Set()
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


  val bacau = Dijkstra.Node("Bacau")
  val iasi = Dijkstra.Node("Iasi")
  val roman = Dijkstra.Node("Roman")
  val piatraNeamt = Dijkstra.Node("Piatra-Neamt")
  val bucuresti = Dijkstra.Node("Bucuresti")
  val brasov = Dijkstra.Node("Brasov")
  val cluj = Dijkstra.Node("Cluj")
  val timisoara = Dijkstra.Node("Timisoara")

  val graph = Dijkstra.Graph(Map(
    Edge(bacau, roman) -> Distance(60),
    Edge(bacau, piatraNeamt) -> Distance(70),
    Edge(roman, iasi) -> Distance(70),
    Edge(piatraNeamt, iasi) -> Distance(80),
    Edge(piatraNeamt, brasov) -> Distance(400),
    Edge(piatraNeamt, cluj) -> Distance(450),
    Edge(cluj, brasov) -> Distance(100),
    Edge(cluj, timisoara) -> Distance(300),
    Edge(brasov, timisoara) -> Distance(250),
    Edge(brasov, bucuresti) -> Distance(450),
    Edge(bacau, bucuresti) -> Distance(300)
  ))
}
