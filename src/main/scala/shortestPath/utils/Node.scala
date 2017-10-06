package shortestPath.utils

import shortestPath.dijkstra.imperative.Dijkstra.{Distance, Edges}

class Node(val name: String,
           var neighbors: Edges = List(),
           var tentativeDistance: Distance = Int.MaxValue,
           var previous: Option[Node] = None) extends Utils {
  def addNeighbors(newNeighbors: List[(Node, Distance)]): Unit = neighbors = neighbors ++ newNeighbors

  def updateNeighborsDijkstra(visited: Set[Node]): Edges = {
    neighbors foreach {
      n =>
        if ((this.tentativeDistance + n._2 < n._1.tentativeDistance) && !visited.contains(n._1)) {
          n._1.previous = Some(this)
          n._1.tentativeDistance = this.tentativeDistance + n._2
        }
    }

    neighbors
  }

  def updateNeighborsBellmanFord(start: Node): Edges = {
    neighbors foreach {
      n =>
        if (this.tentativeDistance + n._2 < n._1.tentativeDistance && this.tentativeDistance != Int.MaxValue && start != n._1) {
          println("Updating from " + this.name + " to " + n._1.name + " with " + this.tentativeDistance + " " + n._2 + " = " + (this.tentativeDistance + n._2))
          n._1.previous = Some(this)
          n._1.tentativeDistance = this.tentativeDistance + n._2
        }
    }

    neighbors
  }
}
