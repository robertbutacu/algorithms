import shortestPath.Dijkstra
import shortestPath.Dijkstra.{Distance, Edge}
import stringOperations._
import stringOperations.examples.StreamsExamples
/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory with StreamsExamples{
  def time[R](block: => R, methodName: String): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"Elapsed time on $methodName: " + (t1 - t0) + "ms")
    result
  }
}
