package shortestPath.dijkstra

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra extends GraphExample{
  case class Node(name: String, tentativeDistance: Option[Int] = None)
  case class Edge(edge: (Node, Node), distance: Int)
  type Graph = List[Edge]

  //initialising all the tentative distances with None, except for the start node
  def initialize(start: Node, graph: Graph): Graph = {
    graph.map(node =>
      if(node.edge._1.name == start.name) Edge((Node(node.edge._1.name, Some(0)),Node(node.edge._2.name)), node.distance)
      else if(node.edge._2.name == start.name) Edge((Node(node.edge._1.name), Node(node.edge._2.name, Some(0))), node.distance)
           else Edge((Node(node.edge._1.name), Node(node.edge._2.name)), node.distance)
    )
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
      val updatedGraph = updateTentativeDistance(
        neighbors(curr, currGraph)
        .filter(node => isTentativeNodeSmaller(curr, node, currGraph))
        .map(node => Node(node.name, Some(curr.tentativeDistance.getOrElse(0) + distanceBetween(curr, node, currGraph).getOrElse(0))))
        .toSet,
        currGraph)
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
        graph.find(e => (e.edge._1.name == curr.name && e.edge._2.name == other.name) || (e.edge._2.name == curr.name && e.edge._1.name == other.name )).get.distance < t
      case None => true
    }
  }

  def updateTentativeDistance(nodes: Set[Node], graph: Graph): Graph = {
    graph.map(n =>
        if(nodes.exists(_.name == n.edge._1.name)) Edge((nodes.find(_.name == n.edge._1.name).get, n.edge._2), n.distance)
        else if(nodes.exists(_.name == n.edge._2.name)) Edge((n.edge._1, nodes.find(_.name == n.edge._2.name).get), n.distance)
        else n
    )
  }

  def isValidGraph(graph: Graph): Boolean = {
    graph.forall(node => node.distance > 0)
  }

  def neighbors(node: Node, graph: Graph): List[Node] = {
    graph.filter(edge => edge.edge._1.name == node.name || edge.edge._2.name == node.name)
      .flatMap(e => List(e.edge._1) ::: List(e.edge._2))
      .filter( _.name != node.name)
  }

  def addEdge(newEdge: Edge, graph: Graph): Option[Graph] = {
    if(graph.exists(e =>
      (e.edge._1.name == newEdge.edge._2.name && e.edge._2.name == newEdge.edge._1.name)
      || (e.edge._1.name == newEdge.edge._1.name && e.edge._2.name == newEdge.edge._2.name))
      || newEdge.distance <= 0
    ) None
    else Some(graph :+ newEdge)
  }

  def distanceBetween(first: Node, second: Node, graph: Graph): Option[Int] = {
    graph.find(e =>
      (e.edge._1.name == first.name && e.edge._2.name == second.name)
      || (e.edge._1.name == second.name && e.edge._2.name == first.name)
    ) match {
      case Some(edge) => Some(edge.distance)
      case None       => None
    }
  }

  def removeEdge(edge: Edge, graph: Graph): Graph = {
    graph.filter(_ != edge)
  }
}
