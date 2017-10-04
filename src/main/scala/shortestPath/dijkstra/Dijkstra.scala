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

  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Int = {

    //TODO nodes that aren't visited are put multiple times in the priority queue
    def go(curr: Node, goalNode: Node, priorityQueue: mutable.MutableList[Node], visited: Set[Node]): Int = {
      if(curr.name == goalNode.name)
        100
      else{
        Thread.sleep(3000)
        println("Current " + curr.name)
        val visitedUpdated = visited ++ Set(curr)

        //updating tentative distances
        curr.updateNeighborsTentativeDistances(visitedUpdated)

        //update priority queueQ
        curr.neighbors map (_._1) filterNot visitedUpdated.contains foreach (node => if(!priorityQueue.contains(node)) priorityQueue += node)

        //removing current from priority list
        val updatedPq = priorityQueue filterNot (_ == curr)

        //re-sorting the queue
        updatedPq sortWith (_.tentativeDistance < _.tentativeDistance)
        go(
          updatedPq.head,
          goalNode,
          updatedPq,
          visitedUpdated
        )
      }
    }

    go(
      start,
      goalNode,
      mutable.MutableList(start),
      Set()
    )
  }
}
