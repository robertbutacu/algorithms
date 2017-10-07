package shortestPath.dijkstra.functional

import scala.annotation.tailrec

object DijkstraFunc {
  type CityName      = String
  type Distance      = Int
  type NodeId        = Int
  type Graph         = Map[Edge, Distance]
  type Path          = (Nodes, Distance)
  type VisitedNodes  = Nodes
  type PriorityQueue = Nodes
  type Nodes         = List[Node]

  case class Node(id: NodeId,
                  name: CityName,
                  tentativeDistance : Distance = Int.MaxValue,
                  previous: Option[Node] = None)

  case class Edge(from: NodeId, to: NodeId)

  def shortest(from: Node, to: Node, nodes: Nodes, graph: Graph): Path = {
    @tailrec
    def go(curr: Node, nodes: Nodes, pq: PriorityQueue, vn: VisitedNodes): Path = {
      if(curr.name == to.name)
        (path(curr) :+ curr, curr.tentativeDistance)
      else{
        println("Currently in " + curr.name + " " + curr.tentativeDistance)

        val neighbors = getNeighbors(curr, graph)

        val updatedVn = vn :+ curr

        val updatedNodes = transformNeighbors(curr, neighbors, nodes)

        val updatedPq = transformPriorityQueue(
          updatedNodes.filter(n => neighbors.keys.toList.contains(n.id)),
          updatedVn,
          pq filterNot(_.name == curr.name))

        go(updatedPq.head,
          updatedNodes,
          updatedPq,
          updatedVn)
      }
    }

    go(from, nodes, List(from), List())
  }

  def path(curr: Node): Nodes = {
    curr.previous match {
      case None       => List()
      case Some(node) => path(node) :+ node
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
    graph filterKeys(e => e.from == node.id) map(c => (c._1.to, c._2))
  }

  def orderPriorityQueue(priorityQueue: PriorityQueue): PriorityQueue = {
    priorityQueue sortWith (_.tentativeDistance < _.tentativeDistance)
  }

  def transformPriorityQueue(nodes: List[Node], visitedNodes: VisitedNodes, priorityQueue: PriorityQueue): PriorityQueue = {
    orderPriorityQueue(priorityQueue ++ (nodes filterNot visitedNodes.contains filterNot priorityQueue.contains))
  }
}
