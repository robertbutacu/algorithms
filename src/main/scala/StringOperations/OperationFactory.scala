package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
object OperationFactory {
  def apply(x: String, y: String, operation: Operation): Either[String, InputException] = {
    operation match {
      case Add      => Left(Addition(x, y))
      case Multiply => Left(Multiplication(x, y))
      case Subtract => Subtraction(x, y)
      case _        => Left("Unknown")
    }
  }
}
