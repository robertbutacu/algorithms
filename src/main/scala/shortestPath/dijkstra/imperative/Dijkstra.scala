package shortestPath.dijkstra.imperative

import shortestPath.utils.{DijkstraGraph, Node}

import scala.collection.mutable

/**
  * Created by Robert-PC on 9/21/2017.
  *
  * Dijkstra's algorithm is an algorithm for finding the shortest paths between nodes in a graph, which may represent, for example, road networks.
  * It works only on graphs with positive weighted edges.
  */
object Dijkstra extends DijkstraGraph {
  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Path = {
    def go(curr: Node, goalNode: Node, priorityQueue: mutable.MutableList[Node], visited: Set[Node]): Path = {
      println("Currently in: " + curr.name)

      //adding current node to visited
      val visitedUpdated = visited ++ Set(curr)

      //updating tentative distances
      curr.updateNeighborsDijkstra(visitedUpdated)

      //update priority queue
      curr.neighbors map (_._1) filterNot visitedUpdated.contains foreach (node => if (!priorityQueue.contains(node)) priorityQueue += node)

      //removing current from priority list
      val updatedPq = priorityQueue filterNot (_ == curr)

      //re-sorting the queue
      updatedPq sortWith (_.tentativeDistance < _.tentativeDistance)

      if (updatedPq.isEmpty || curr == goalNode)
        (path(goalNode), goalNode.tentativeDistance)
      else
        go(
          updatedPq.head,
          goalNode,
          updatedPq,
          visitedUpdated
        )
    }

    initialize(start, graph)

    go(
      start,
      goalNode,
      mutable.MutableList(start),
      Set()
    )
  }
}
