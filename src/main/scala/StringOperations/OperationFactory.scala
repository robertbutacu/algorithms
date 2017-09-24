package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory {
  def compute(x: String, operation: Operation, y: String): Either[String, InputException] = {
    isValid(x, y, operation) match {
      case Valid                          => handleComputation(x, y, operation)
      case InvalidInputException(msg)     => Right(InvalidInputException(msg))
    }
  }

  private def handleComputation(x: String, y: String, operation: Operation): Either[String, InputException] = {
    getSignums(x, y) match {
      case NoNegativeOperands    =>
        if(y > x && operation == Subtract)
          Left("-" ++ Subtraction(y, x))
        else Left(executeComputation(x, y, operation))

      case NegativeLeftOperand   =>
        handleNegativeLeftOperand(x.drop(1), y, operation)

      case NegativeRightOperand  =>
        handleNegativeRightOperand(x, y.drop(1), operation)

      case BothOperandsNegative  =>
        handleBothOperandsNegative(x.drop(1), y.drop(1), operation)
    }
  }

  private def executeComputation(x: String, y: String, operation: Operation): String = {
    operation match {
      case Add      => Addition(x, y)
      case Multiply => Multiplication(x, y)
      case Subtract => Subtraction(x, y)
      case Pow      => Exponentiation(x, y)
      case Divide   => "Unknown"
    }
  }

  private def handleBothOperandsNegative(x: String, y: String, operation: Operation): Either[String, InputException] = {
    operation match {
      case Add      =>
        Left("-" ++ Addition(x, y))
      case Subtract =>
        if(x > y) Left("-" ++ Subtraction(x, y))
        else      Left(Subtraction(y, x))
      case Multiply =>
        Left(Multiplication(x, y))
      case Divide   =>
        if ( y.drop(1).dropWhile(_.eq('0')).isEmpty)
          Right(InvalidInputException("Divisor is 0!"))
        else
          Left(Division(x, y))
      case Pow      =>
        Right(InvalidInputException(""))
    }
  }

  private def handleNegativeLeftOperand(x: String, y: String, operation: Operation): Either[String, InputException] = {
    operation match {
      case Add      =>
        if (x > y) Left("-" ++ Subtraction(x, y))
        else Left(Subtraction(y, x))
      case Subtract => Left("-" ++ Addition(x, y))
      case Multiply => Left("-" ++ Multiplication(x, y))
      case Divide   =>
        if ( y.dropWhile(_.eq('0')).isEmpty )
          Right(InvalidInputException("Divisor is 0!"))
        else
          Left("-" ++ Division(x, y))
      case Pow      => Right(InvalidInputException("Not implemented!"))
    }
  }

  private def handleNegativeRightOperand(x: String, y: String, operation: Operation): Either[String, InputException] = {
    operation match {
      case Add      =>
        if(x > y) Left(Subtraction(x, y))
        else      Left("-" ++ Subtraction(y, x))
      case Subtract => Left(Addition(x, y))
      case Multiply => Left("-" ++ Multiplication(x, y))
      case Divide   => Right(InvalidInputException("Not implemented!"))
      case Pow      => Right(InvalidInputException("Not implemented!"))
    }
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

  private def isValid(x: String, y: String, op: Operation): InputException = {
    if(isDigitsOnly(x.filter(!_.equals('-')), y.filter(!_.equals('-')))) {
      if (isSignumCorrect(x, op) && isSignumCorrect(y, op))
        Valid
      else
        InvalidInputException(s"""Signum position is incorrect -> $x, $y !""")
    }
    else
      InvalidInputException(s"""Please provide numbers with digits only -> $x, $y !""")
  }
}
