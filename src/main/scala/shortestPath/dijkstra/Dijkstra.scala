package shortestPath.dijkstra

import shortestPath.utils.{DijkstraGraph, Node}
import scala.collection.mutable

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra extends DijkstraGraph {
  type Graph = List[Node]
  type Distance = Int
  type Edges = List[(Node, Distance)]

  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Int = {
    def go(curr: Node, goalNode: Node, priorityQueue: mutable.MutableList[Node], visited: Set[Node]): Int = {
      Thread.sleep(3000)
      println("Current " + curr.name)

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
        goalNode.tentativeDistance
      else
        go(
          updatedPq.head,
          goalNode,
          updatedPq,
          visitedUpdated
        )
    }

    go(
      start,
      goalNode,
      mutable.MutableList(start),
      Set()
    )
  }
}
