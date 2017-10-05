
import shortestPath.dijkstra.Dijkstra
import shortestPath.utils.GraphExample
import stringOperations._
import stringOperations.examples.StreamsExamples

import scala.collection.mutable

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

  println(Dijkstra.shortest(bacau, bucuresti, getGraph))
  //println( (1 to 10) filterNot ( _ % 2 == 0))
  /*var pq = mutable.PriorityQueue[Int]()

  pq.enqueue(1)
  pq.enqueue(2)
  pq.enqueue(3)
  pq.enqueue(4)

  pq.foreach(e => println(e))

  pq.dequeue()

  println()
  pq.foreach(e => println(e))

  if(pq.exists(_ == 2))
    println("Exists!")
  else
    pq.enqueue(5)

  println("Head " + pq.head )*/


}
