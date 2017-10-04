package shortestPath.dijkstra

import shortestPath.dijkstra.Dijkstra.Distance

class Node(val name: String,
           var neighbors: Set[(Node, Distance)] = Set(),
           var tentativeDistance: Distance = Int.MaxValue) {
  def addNeighbors(newNeighbors: Set[(Node, Distance)]) = neighbors = neighbors ++ newNeighbors
}
