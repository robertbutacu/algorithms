
import shortestPath.dijkstra.imperative.Dijkstra
import shortestPath.utils.{BellmanFordGraph, DijkstraGraph}
import stringOperations._
import stringOperations.examples.StreamsExamples

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

  val shortest = Dijkstra.shortest(bacau, timisoara, getDjikGraph)
  println("The path is: ")
  shortest._1.foreach(e => println(e.name))
  println(shortest._2)

  getDjikGraph.foreach(n => println(n.tentativeDistance))
}
