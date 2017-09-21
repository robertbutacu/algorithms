package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
object OperationFactory {
  def apply(x: String, y: String, operation: Operation): String = {
    operation match {
      case Add => Addition(x, y)
      case _   => "Unknown operation!"
    }
  }
}
