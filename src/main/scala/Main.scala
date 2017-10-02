
import shortestPath.dijkstra.{Dijkstra, GraphExample}
import shortestPath.dijkstra.Dijkstra.Edge
import stringOperations._
import stringOperations.examples.StreamsExamples
/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory with StreamsExamples with GraphExample{
  def time[R](block: => R, methodName: String): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"Elapsed time on $methodName: " + (t1 - t0) + "ms")
    result
  }

  //println(Dijkstra.graph)
  //println(Dijkstra.addOrUpdateEdge(Edge(Dijkstra.bacau, Dijkstra.roman), Distance(1000), Dijkstra.graph))
  //println(Dijkstra.removeEdge(Edge((bacau, roman), 60), graph))
  println(Dijkstra.isTentativeNodeSmaller(bacau, roman, graph))
}
