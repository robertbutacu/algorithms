package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory {
  def compute(x: String, y: String, operation: Operation): Option[String] = {
    def executeComputation(x: String, y: String, operation: Operation): String = {
      operation match {
        case Add      => Addition(x, y)
        case Multiply => Multiplication(x, y)
        case Subtract => Subtraction(x, y)
        case Pow      => Exponentiation(x, y)
        case Divide   => "Unknown"
      }
    }

    if (isValid(x, y, operation))
      getSignums(x, y) match {
        case NoNegativeOperands      =>
          Some(executeComputation(x, y, operation))

        case NegativeLeftOperand   =>
          if (operation == Subtract) Some("-" ++ executeComputation(x.drop(1), y, Add))
          else Some("-" ++ executeComputation(x.drop(1), y, operation))

        case NegativeRightOperand  =>
          if (operation == Subtract) Some(executeComputation(x, y, Add))
          else Some("-" ++ executeComputation(x, y.drop(1), operation))

        case BothOperandsNegative =>
          if (operation == Subtract) Some(executeComputation(y.drop(1), x.drop(1), Subtract))
          else Some(executeComputation(x.drop(1), y.drop(1), operation))
      }

    else
      None
  }

  private def getSignums(x: String, y: String): Signum = {
    (x.charAt(0), y.charAt(0)) match {
      case ('-', '-') => BothOperandsNegative
      case ('-', _)   => NegativeLeftOperand
      case (_, '-')   => NegativeRightOperand
      case _          => NoNegativeOperands
    }
  }

  private def isDigitsOnly(x: String, y: String): Boolean = {
    x.forall(_.isDigit) && y.forall(_.isDigit)
  }

  private def isSignumCorrect(x: String, operation: Operation): Boolean = {
    x.forall(_.isDigit) || (x.startsWith("-") && (x.count(_.equals('-')) == 1) && operation != Pow)
  }

  private def isValid(x: String, y: String, op: Operation) = {
    isDigitsOnly(x.filter(!_.equals('-')), y.filter(!_.equals('-'))) &&
      isSignumCorrect(x, op) &&
      isSignumCorrect(y, op)
  }
}
