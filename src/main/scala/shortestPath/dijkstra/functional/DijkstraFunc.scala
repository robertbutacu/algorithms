package shortestPath.dijkstra.functional

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  *
  * Dijkstra's algorithm is an algorithm for finding the shortest paths between nodes in a graph, which may represent, for example, road networks.
  * It works only on graphs with positive weighted edges.
  *
  *   In order to assure immutability as easy as possible, the graph itself is represented as a list of Edges composed of 2 IDs of nodes,
  * instead of the nodes themselves, which are being stored in a list.
  *   This way, if a tentative distance is to be updated, it will be updated in a single place, not for every appearance of the node in the graph.
  *   -> true for prevNode also.
  *   Thus, it is easier to keep track of the state of the nodes, and also to output the final path to the destination node.
  */

object DijkstraFunc {
  type CityName      = String
  type Distance      = Int
  type NodeId        = Int
  type Graph         = Map[Edge, Distance]
  type Path          = (Nodes, Distance)
  type VisitedNodes  = Nodes
  type PriorityQueue = List[Node]
  type Nodes         = Set[Node]

  case class Node(id: NodeId,
                  name: CityName,
                  tentativeDistance: Distance = Int.MaxValue,
                  previous: Option[Node] = None)

  case class Edge(from: NodeId, to: NodeId)

  def shortest(from: Node, to: Node, nodes: Nodes, graph: Graph): Path = {
    @tailrec
    def go(curr: Node, nodes: Nodes, pq: PriorityQueue, vn: VisitedNodes): Path = {
      if (curr.name == to.name)
        (path(curr) + curr, curr.tentativeDistance)
      else {
        println("Currently in " + curr.name + " " + curr.tentativeDistance)

        val neighbors = getNeighbors(curr, graph)

        val updatedVn = vn + curr

        val updatedNodes = transformNeighbors(curr, neighbors, nodes)

        val updatedPq = transformPriorityQueue(
          updatedNodes filter (n => neighbors.keys.toList.contains(n.id)),
          updatedVn,
          pq filterNot (_.name == curr.name))

        go(updatedPq.head,
          updatedNodes,
          updatedPq,
          updatedVn)
      }
    }

    go(from, nodes, List(from), Set())
  }

  def path(curr: Node): Nodes = {
    curr.previous match {
      case None       => Set()
      case Some(node) => path(node) + node
    }
  }

  def transformNeighbors(curr: Node, neighbors: Map[NodeId, Distance], nodes: Nodes): Nodes = {
    nodes.map(n =>
      if (neighbors.keys.toList.contains(n.id) && (neighbors(n.id) + curr.tentativeDistance < n.tentativeDistance))
        Node(n.id, n.name, neighbors(n.id) + curr.tentativeDistance, Some(curr))
      else
        n
    )
  }

  def getNeighbors(node: Node, graph: Graph): Map[NodeId, Distance] = {
    graph filterKeys (e => e.from == node.id) map (c => (c._1.to, c._2))
  }

  def orderPriorityQueue(priorityQueue: PriorityQueue): PriorityQueue = {
    priorityQueue sortWith (_.tentativeDistance < _.tentativeDistance)
  }

  def transformPriorityQueue(nodes: Set[Node], visitedNodes: VisitedNodes, priorityQueue: PriorityQueue): PriorityQueue = {
    orderPriorityQueue(priorityQueue ++ (nodes diff visitedNodes filterNot priorityQueue.contains))
  }
}
