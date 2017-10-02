package shortestPath

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra {
  case class Distance(dist: Int)
  case class Node(name: String, tentativeDistance: Option[Int] = None)
  case class Edge(connection: (Node, Node))
  case class Graph(nodes: Map[Edge, Distance])

  def initialize(start: Node, graph: Graph): Graph = {
    Graph(graph.nodes.map(node =>
      if(node._1.connection._1 == start) Edge(Node(node._1.connection._1.name, Some(0)), Node(node._1.connection._2.name, None)) -> node._2
      else if(node._1.connection._2 == start) Edge(Node(node._1.connection._1.name, None),Node(node._1.connection._2.name, Some(0))) -> node._2
      else Edge(Node(node._1.connection._1.name, None), Node(node._1.connection._2.name, None)) -> node._2
    ))
  }

  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Option[Set[Node]] = {
    //@tailrec
    def go(curr: Node, goalNode: Node, visited: Set[Node], path: Set[Node]): Option[Set[Node]] = {
      /** 1. for the current node, compute the the tentative distance to all its neighbours as min((dist to curr node) + (dist to that node),(neighbor's tentative distance)
          2. mark current node as visited
          3. unvisited node with the smallest tentative distance as current node and repeat
        */
      graph.nodes.keys
        .filter(conn => conn.connection._1 == curr || conn.connection._2 == curr)//currently a graph with only current nodes
      None
    }

    go(
      start,
      goalNode,
      Set.empty[Node],
      Set.empty[Node]
    )
  }

  def addOrUpdateEdge(newEdge: Edge, distance: Distance, graph: Graph): Option[Graph] = {
    if(graph.nodes.exists(e =>
        e._1.connection._1 == newEdge.connection._2 && e._1.connection._2 == newEdge.connection._1)
      || distance.dist <= 0
    ) None
    else Some(Graph(graph.nodes + (newEdge -> distance)))
  }

  def removeEdge(edge: Edge, distance: Distance, graph: Graph): Graph = {
    Graph(graph.nodes - edge)
  }

  def isValidGraph(graph: Graph): Boolean = {
    graph.nodes.values.forall( _.dist > 0)
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
    Edge((bacau, roman)) -> Distance(60),
    Edge((bacau, piatraNeamt)) -> Distance(70),
    Edge((roman, iasi)) -> Distance(70),
    Edge((piatraNeamt, iasi)) -> Distance(80),
    Edge((piatraNeamt, brasov)) -> Distance(400),
    Edge((piatraNeamt, cluj)) -> Distance(450),
    Edge(cluj, brasov) -> Distance(100),
    Edge(cluj, timisoara) -> Distance(300),
    Edge(brasov, timisoara) -> Distance(250),
    Edge(brasov, bucuresti) -> Distance(450),
    Edge(bacau, bucuresti) -> Distance(300)
  ))
}
