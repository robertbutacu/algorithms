package shortestPath.dijkstra.functional

object Dijkstra {
  type CityName = String
  type Distance = Int
  type Graph    = Map[Edge, Distance]
  case class Node(name: CityName, tentativeDistance : Option[Distance] = None)
  case class Edge(from: Node, to: Node)

}
