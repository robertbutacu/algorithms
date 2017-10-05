package shortestPath.bellmanford

import shortestPath.utils.{BellmanFordGraph, Node}

/**
  * Created by Robert-PC on 9/21/2017.
  * The Bellman–Ford algorithm is an algorithm that computes shortest paths from a single source vertex
  * to all of the other vertices in a weighted digraph.
  * It is slower than Dijkstra's algorithm for the same problem,
  * but more versatile, as it is capable of handling graphs in which some of the edge weights are negative numbers.
  */
object BellmanFord extends BellmanFordGraph {
  def shortest(start: Node, goalNode: Node, graph: Graph): Path = {
    def go(iteration: Int, graph: Graph): Path = {
      if (iteration == graph.size)
        path(goalNode)
      else {
        graph foreach (_.updateNeighborsBellmanFord(start))
        go(iteration + 1, graph)
      }
    }

    initialize(start, graph)
    go(1, graph sortWith (_.tentativeDistance < _.tentativeDistance))
  }
}
