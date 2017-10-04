package shortestPath.dijkstra

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Dijkstra extends GraphExample{
  type Graph = List[Node]
  type Distance = Int
  //initialising all the tentative distances with None, except for the start node


  def shortest(start: Node,
               goalNode: Node,
               graph: Graph): Option[Int] = {
    Some(0)
  }
}
