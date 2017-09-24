import StringOperations._

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory{
  println(compute(Some("2"), Add, Some("-10")))
}
