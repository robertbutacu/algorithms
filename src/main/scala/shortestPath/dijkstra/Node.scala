package shortestPath.dijkstra

import shortestPath.dijkstra.Dijkstra.Distance

class Node(val name: String,
           var neighbors: List[(Node, Distance)] = List(),
           var tentativeDistance: Distance = Int.MaxValue) {
  def addNeighbors(newNeighbors: List[(Node, Distance)]) = neighbors = neighbors ++ newNeighbors
}
