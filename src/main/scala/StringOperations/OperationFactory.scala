package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory {
  def compute(x: String, y: String, operation: Operation): Option[String] = {
     def executeComputation(x: String, y: String, operation: Operation): Option[String] = {
      operation match {
        case Add      => Addition(x, y)
        case Multiply => Multiplication(x, y)
        case Subtract => Subtraction(x, y)
        case Pow      => Exponentiation(x, y)
        case _        => None
      }
    }

    executeComputation(x, y, operation)
  }

  private def isValid(x: String, y: String): Boolean = {
    x.forall(_.isDigit) && y.forall(_.isDigit)
  }
}
