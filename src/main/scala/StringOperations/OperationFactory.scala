package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
object OperationFactory {
  def apply(x: String, y: String, operation: Operation): Option[String] = {
    operation match {
      case Add      => Addition(x, y)
      case Multiply => Multiplication(x, y)
      case Subtract => Subtraction(x, y)
      case Pow      => Exponentiation(x, y)
      case _        => None
    }
  }
}
