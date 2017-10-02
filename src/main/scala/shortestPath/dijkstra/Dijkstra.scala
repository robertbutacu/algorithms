package shortestPath.dijkstra

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra extends GraphExample{
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
    def go(curr: Node, goalNode: Node, currGraph: Graph, visited: Set[Node], path: Set[Node]): Option[Int] = {
      /** 1. for the current node, compute the the tentative distance to all its neighbours as min((dist to curr node) + (dist to that node),(neighbor's tentative distance)
          2. mark current node as visited
          3. unvisited node with the smallest tentative distance as current node and repeat
        */
      //var updatedGraph
      val updatedGraph = neighbors(curr, currGraph)
        //.filter(node => isTentativeNodeSmaller(curr, node, currGraph))
      println(updatedGraph)
      None
    }

    if(isValidGraph(graph))
      go(
        start,
        goalNode,
        initialize(start, graph),
        Set.empty[Node],
        Set.empty[Node]
      )
    else
      None
  }

  /**
    *
    * @param curr - current node
    * @param other - node which it is tried to reach
    * @param graph - graph on which the shortest path is to be found
    * @return true IF distance to curr node + distance to other node is less than tentative distance of the other node
    */
  def isTentativeNodeSmaller(curr: Node, other:Node, graph: Graph): Boolean = {
    other.tentativeDistance match {
      case Some(t) => curr.tentativeDistance.get +
        graph.nodes
          .find(e => (e.edge._1.name == curr.name && e.edge._2.name == other.name) || (e.edge._2.name == curr.name && e.edge._1.name == other.name )).get.distance < t
      case None => true
    }
  }

  def updateTentativeDistance(nodes: Set[Node], graph: Graph): Graph = {
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
      .filter(edge => edge.edge._1.name == node.name || edge.edge._2.name == node.name)
      .flatMap(e => List(e.edge._1) ::: List(e.edge._2))
      .filter( _.name != node.name)
  }

  def addEdge(newEdge: Edge, graph: Graph): Option[Graph] = {
    if(graph.nodes.exists(e =>
      (e.edge._1.name == newEdge.edge._2.name && e.edge._2.name == newEdge.edge._1.name)
      || (e.edge._1.name == newEdge.edge._1.name && e.edge._2.name == newEdge.edge._2.name))
      || newEdge.distance <= 0
    ) None
    else Some(Graph(graph.nodes :+ newEdge))
  }

  def removeEdge(edge: Edge, graph: Graph): Graph = {
    Graph(graph.nodes.filter(_ != edge))
  }
}
