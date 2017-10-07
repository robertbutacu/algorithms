package shortestPath.dijkstra.functional

import java.util.UUID

import scala.annotation.tailrec

object DijkstraFunc {
  type CityName      = String
  type Distance      = Int
  type NodeId        = Int
  type Graph         = Map[NodeId, Distance]
  type Path          = (List[Edge], Distance)
  type VisitedNodes  = List[NodeId]
  type PriorityQueue = Nodes
  type Nodes         = List[Node]
  case class Node(id: NodeId = UUID.randomUUID().toString.toInt,
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

  def transformNeighbors(graph: Graph, nodes: Nodes): Graph = {
    Map[NodeId, Distance]().empty
  }

  def transformEdge(edge: Edge, distance: Distance): Edge = {
    Edge(0 , 0)
  }

  def orderPriorityQueue(priorityQueue: PriorityQueue): PriorityQueue = {
    List()
  }

  def transformPriorityQueue(nodes: List[Node], visitedNodes: VisitedNodes, priorityQueue: PriorityQueue): PriorityQueue = {
    List()
  }
}
