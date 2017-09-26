import stringOperations._
import stringOperations.examples.StreamsExamples
import stringOperations.operations.Mod
import stringOperations.utils._

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory with StreamsExamples{
  //println(compute(Some(Neg("2")), Multiply, Some(Pos("3"))))
  //println(squaresUpUntil(Pos("10")).toList)
  //println(squaresBetween(Pos("124"), Pos("1000")).toList)

  //println("0")
  println(Mod("10", "2"))
  //println(compute(Some(Pos("2")), Subtract, Some(Pos("2"))))
}
