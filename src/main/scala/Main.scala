import StringOperations._

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory with StreamsExamples{
  //println(compute(Some(Neg("2")), Add, Some(Pos("3"))))
  println(squaresUpUntil(Pos("10")).toList)
}
