package shortestPath.dijkstra.functional

import java.util.UUID

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

  def shortest(from: Node, to: Node, graph: Graph): Path = {
    //@tailrec
    def go(curr: Node, currGraph: Graph, pq: PriorityQueue, vn: VisitedNodes): Path = {
      /**
        * add curr to visited list
        *
        * updated current node's neighbors
        *
        * update priority queue
        *
        * recursive call to go until curr is equal to to
        */
      (List(), 0)
    }

    go(from, graph, List(), List())
  }

  //def path(goal: Node, graph: Graph)

  def transformNeighbors(node: Node, graph: Graph, nodes: Nodes): Graph = {
    Map[Edge, Distance]().empty
  }

  def getNeighbors(node: Node, graph: Graph): List[(NodeId, Distance)] = {
    (graph filterKeys(e => e.from == node.id) map(c => (c._1.to, c._2))).toList
  }

  def orderPriorityQueue(priorityQueue: PriorityQueue): PriorityQueue = {
    priorityQueue sortWith (_.tentativeDistance < _.tentativeDistance)
  }

  def transformPriorityQueue(nodes: List[Node], visitedNodes: VisitedNodes, priorityQueue: PriorityQueue): PriorityQueue = {
    orderPriorityQueue(priorityQueue ++ nodes filterNot visitedNodes.contains filterNot priorityQueue.contains)
  }
}
