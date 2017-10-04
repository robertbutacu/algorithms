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
               graph: Graph): Int = {
    def go(curr: Node, goalNode: Node, priorityQueue: mutable.PriorityQueue[Node], visited: Set[Node]): Int = {
      if(curr.name == goalNode.name)
        100
      else{
        //updating tentative distances
        Thread.sleep(3000)
        println("Current is " + curr.name)
        val visitedUpdated = visited ++ Set(curr)

        //update priority queue
        curr.neighbors.map (_._1) filterNot visited.contains foreach (priorityQueue.enqueue(_))

        priorityQueue.reverse.dequeue()
        println("Next is " + priorityQueue.reverse.head.name)
        go(
          priorityQueue.reverse.head,
          goalNode,
          priorityQueue,
          visitedUpdated
        )
      }
    }

    go(
      start,
      goalNode,
      mutable.PriorityQueue[Node]()(Ordering.by(_.tentativeDistance)),
      Set()
    )
  }
}
