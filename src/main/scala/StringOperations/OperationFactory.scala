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

    if(isDigitsOnly(x, y) && isSignumCorrect(x) && isSignumCorrect(y))
      executeComputation(x, y, operation)
    else
      None
  }

  private def getSignum(x: String, y: String): Signum = {
    (x.charAt(0), y.charAt(0)) match {
      case _ => BothOperandsAreNegative
    }
  }

  private def isDigitsOnly(x: String, y: String): Boolean = {
    x.forall(_.isDigit) && y.forall(_.isDigit)
  }

  private def isSignumCorrect(x: String): Boolean = {
    x.forall(_.isDigit) || (x.startsWith("-") && ( x.count(_.equals('-')) == 1))
  }
}
