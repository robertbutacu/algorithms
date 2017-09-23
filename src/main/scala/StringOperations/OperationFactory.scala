package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory {
  def compute(x: String, y: String, operation: Operation): Option[String] = {
    def executeComputation(x: String, y: String, operation: Operation): Option[String] = {
      operation match {
        case Add => Some(Addition(x, y))
        case Multiply => Some(Multiplication(x, y))
        case Subtract => Some(Subtraction(x, y))
        case Pow => Some(Exponentiation(x, y))
        case _ => None
      }
    }

    if (
      isDigitsOnly(x.filter(_.equals('-')), y.filter(_.equals('-'))) &&
        isSignumCorrect(x) &&
        isSignumCorrect(y)
    )
      getSignum(x, y) match {
        case NoNegativeOperands => executeComputation(x, y, operation)
        case LeftOperandIsNegative =>
          if (operation == Subtract) Some("-" :: executeComputation(x, y, Add))
          else executeComputation(x, y, operation)
        case RightOperandIsNegative => executeComputation(x, y, operation)
        case BothOperandsAreNegative => executeComputation(x, y, operation)
      }

    else
      None
  }

  private def getSignum(x: String, y: String): Signum = {
    (x.charAt(0), y.charAt(0)) match {
      case ('-', '-') => BothOperandsAreNegative
      case ('-', _) => LeftOperandIsNegative
      case (_, '-') => RightOperandIsNegative
      case _ => NoNegativeOperands
    }
  }

  private def isDigitsOnly(x: String, y: String): Boolean = {
    x.forall(_.isDigit) && y.forall(_.isDigit)
  }

  private def isSignumCorrect(x: String): Boolean = {
    x.forall(_.isDigit) || (x.startsWith("-") && (x.count(_.equals('-')) == 1))
  }
}
