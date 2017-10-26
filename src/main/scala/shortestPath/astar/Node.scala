package shortestPath.astar

class Node(val state: List[Int],
           var previousState: Option[Node],
           var neighbors: List[Node] = List.empty,
           var distanceToFinal: Int = Int.MaxValue
          ) {
  override def toString: String = this.state.toString()
}
