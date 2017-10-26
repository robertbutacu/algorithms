
import shortestPath.astar.AStar
import shortestPath.dijkstra.functional.GraphExample
import stringOperations._

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
  println(AStar.solveTowerOfHanoi(3, 3))

}
