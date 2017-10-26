
import shortestPath.dijkstra.functional.{DijkstraFunc, GraphExample}
import shortestPath.dijkstra.imperative.Dijkstra
import shortestPath.utils.{BellmanFordGraph, DijkstraGraph}
import stringOperations._
import stringOperations.examples.StreamsExamples
import towerOfHanoi.backtracking.Backtracking
import towerOfHanoi.utils.First

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory
  with GraphExample{
  def time[R](block: => R, methodName: String): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"Elapsed time on $methodName: " + (t1 - t0) + "ms")
    result
  }
  println(Backtracking.solveHanoi(3, 3, First))

}
