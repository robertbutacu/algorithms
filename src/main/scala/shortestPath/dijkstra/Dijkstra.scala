package shortestPath.dijkstra

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
      Thread.sleep(2000)
      println("Currently in: " + curr.name)

      //adding current node to visited
      val visitedUpdated = visited ++ Set(curr)

      //updating tentative distances
      curr.updateNeighborsTentativeDistances(visitedUpdated)

      //update priority queue
      curr.neighbors map (_._1) filterNot visitedUpdated.contains foreach (node => if (!priorityQueue.contains(node)) priorityQueue += node)

      //removing current from priority list
      val updatedPq = priorityQueue filterNot (_ == curr)

      //re-sorting the queue
      updatedPq sortWith (_.tentativeDistance < _.tentativeDistance)

      if (updatedPq.isEmpty || curr == goalNode)
        path(goalNode)
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

  def initialize(start: Node, graph: Graph): Unit = {
    graph foreach { node =>
      if (node == start)
        node.tentativeDistance = 0
      else
        node.tentativeDistance = Int.MaxValue
      node.previous = None
    }
  }

  def path(start: Node): List[Node] = {
    start.previous match {
      case Some(node) => path(node) ::: List(start)
      case None       => List(start)
    }
  }
}
