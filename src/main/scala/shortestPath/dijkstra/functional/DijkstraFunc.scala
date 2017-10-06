package shortestPath.dijkstra.functional

import scala.annotation.tailrec

object DijkstraFunc {
  type CityName      = String
  type Distance      = Int
  type Graph         = Map[Edge, Distance]
  type Path          = (List[Edge], Distance)
  type VisitedNodes  = List[Node]
  type PriorityQueue = List[Node]
  case class Node(name: CityName, tentativeDistance : Distance = Int.MaxValue, previous: Option[Node] = None)
  case class Edge(from: Node, to: Node)

  def shortest(from: Node, to: Node, graph: Graph): Path = {
    @tailrec
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
      val updatedVn = pq :+ curr

      val updatedGraph = transformNeighbors(currGraph)

      val updatedPq = transformPriorityQueue(
        (updatedGraph filterKeys(e => e.from.name == curr.name) map (r => r._1.to)).toList,
        updatedVn,
        pq)

      if(curr.name == to.name) (List(), curr.tentativeDistance)
      else go(updatedPq.head, updatedGraph, updatedPq, updatedVn)
    }

    go(from, graph, List(), List())
  }

  //def path(goal: Node, graph: Graph)

  def transformNeighbors(graph: Graph): Graph = {
    graph.map(r => (transformEdge(r._1, r._2), r._2))
  }

  def transformEdge(edge: Edge, distance: Distance): Edge = {
    val tentDist = edge.from.tentativeDistance + distance
    if(tentDist < edge.to.tentativeDistance) Edge(edge.from, Node(edge.to.name, tentDist, Some(edge.from)))
    else                                     edge
  }

  def orderPriorityQueue(priorityQueue: PriorityQueue): PriorityQueue = {
    priorityQueue sortWith (_.tentativeDistance < _.tentativeDistance)
  }

  def transformPriorityQueue(nodes: List[Node], visitedNodes: VisitedNodes, priorityQueue: PriorityQueue): PriorityQueue = {
    orderPriorityQueue(priorityQueue ++ nodes filterNot visitedNodes.contains filterNot priorityQueue.contains)
  }
}
