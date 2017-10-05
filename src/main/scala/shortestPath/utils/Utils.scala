package shortestPath.utils

trait Utils {
  type Graph = List[Node]
  type Distance = Int
  type Edges = List[(Node, Distance)]
  type Path = List[Node]
}
