package shortestPath.dijkstra

import shortestPath.dijkstra.Dijkstra.{Distance, Graph}

class Node(val name: String,
           var neighbors: List[(Node, Distance)] = List(),
           var tentativeDistance: Distance = Int.MaxValue) {
  def addNeighbors(newNeighbors: List[(Node, Distance)]): Unit = neighbors = neighbors ++ newNeighbors

  def updateNeighborsTentativeDistances(visited: List[Node]): List[(Node, Distance)] = {
    neighbors.foreach{
      n =>
        if(n._1.tentativeDistance > this.tentativeDistance + n._2 && !visited.contains(n._1))
          n._1.tentativeDistance = this.tentativeDistance + n._2
    }

    neighbors
  }
}
