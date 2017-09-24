import StringOperations._

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory{
  println(compute(compute(Some("2"), Subtract, Some("3")), Multiply, Some("10")))
}
