package shortestPath.dijkstra

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra extends GraphExample{
  type Graph = List[Node]
  type Distance = Int
  type Edges = List[(Node, Distance)]
  //initialising all the tentative distances with None, except for the start node


  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Option[Int] = {
    def go(curr: Node, goalNode: Node, priorityQueue: mutable.PriorityQueue[Node], visited: List[Node]): Int = {
      if(curr == goalNode)
        100
      else{
        //updating tentative distances
        curr.updateNeighborsTentativeDistances(visited)
        updatePriorityQueue(curr.neighbors.map(_._1), priorityQueue, visited)
        10
      }
    }

    Some(0)
  }

  def updatePriorityQueue(neighbors: List[Node],
                          priorityQueue: mutable.PriorityQueue[Node],
                          visited: List[Node]): Unit = {
    neighbors filterNot visited.contains foreach (priorityQueue.enqueue(_))
  }
}
