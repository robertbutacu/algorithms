package shortestPath.dijkstra

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra {
  case class Node(name: String, tentativeDistance: Option[Int] = None)
  case class Edge(edge: (Node, Node), distance: Int)
  case class Graph(nodes: List[Edge])

  //initialising all the tentative distances with None, except for the start node
  def initialize(start: Node, graph: Graph): Graph = {
    Graph(graph.nodes.map(node =>
      if(node.edge._1 == start) Edge((Node(node.edge._1.name, Some(0)),Node(node.edge._2.name, None)), node.distance)
      else if(node.edge._2 == start) Edge((Node(node.edge._1.name), Node(node.edge._2.name, Some(0))), node.distance)
           else Edge((Node(node.edge._1.name, None), Node(node.edge._2.name, None)), node.distance)
    ))
  }

  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Option[Int] = {
    //@tailrec
    def go(curr: Node, goalNode: Node, visited: Set[Node], path: Set[Node]): Option[Int] = {
      /** 1. for the current node, compute the the tentative distance to all its neighbours as min((dist to curr node) + (dist to that node),(neighbor's tentative distance)
          2. mark current node as visited
          3. unvisited node with the smallest tentative distance as current node and repeat
        */
      //var updatedGraph
      //neighbors(curr, graph).filter()
      println(graph)
      None
    }

    if(isValidGraph(graph))
      go(
        start,
        goalNode,
        Set.empty[Node],
        Set.empty[Node]
      )
    else
      None
  }

  def updateTentativeDistance(nodes: List[Node], graph: Graph): Graph = {
    Graph(graph.nodes
      .map(n =>
        if(nodes.exists(_.name == n.edge._1.name)) Edge((nodes.find(_.name == n.edge._1.name).get, n.edge._2), n.distance)
        else if(nodes.exists(_.name == n.edge._2.name)) Edge((n.edge._1, nodes.find(_.name == n.edge._2.name).get), n.distance)
        else n
      ))
  }

  def isValidGraph(graph: Graph): Boolean = {
    graph.nodes.forall(node => node.distance > 0)
  }

  def neighbors(node: Node, graph: Graph): List[Node] = {
    graph.nodes
      .filter(edge => edge.edge._1 == node || edge.edge._2 == node)
      .flatMap(e => List(e.edge._1) ::: List(e.edge._2))
      .filter( _ != node)
  }

  def addOrUpdateEdge(newEdge: Edge, graph: Graph): Option[Graph] = {
    if(graph.nodes.exists(e =>
      e.edge._1 == newEdge.edge._2 && e.edge._2 == newEdge.edge._1)
      || newEdge.distance <= 0
    ) None
    else Some(Graph(graph.nodes :+ newEdge))
  }

  def removeEdge(edge: Edge, graph: Graph): Graph = {
    Graph(graph.nodes.filter(_ != edge))
  }
}
