import StringOperations._

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory{
  println(compute(Some(Negative("21231231412352312312")), Add, Some(Positive("11231231231231230"))))
}
