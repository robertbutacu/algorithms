
import shortestPath.bellmanford.BellmanFord
import shortestPath.dijkstra.Dijkstra
import shortestPath.dijkstra.Dijkstra.Path
import shortestPath.utils.{BellmanFordGraph, DijkstraGraph, Node}
import stringOperations._
import stringOperations.examples.StreamsExamples
import stringOperations.utils.{Multiply, Pos, Pow}

import scala.collection.mutable

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory with StreamsExamples with DijkstraGraph with BellmanFordGraph{
  def time[R](block: => R, methodName: String): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"Elapsed time on $methodName: " + (t1 - t0) + "ms")
    result
  }
}
