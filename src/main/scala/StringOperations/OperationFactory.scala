package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory {
  def compute(x: Option[String], operation: Operation, y: Option[String]): Option[String] = {
    if (isValid(x, y, operation))
      handleComputation(x.get, y.get, operation)
    else
      None
  }

  private def handleComputation(x: String, y: String, operation: Operation): Option[String] = {
    getSignums(x, y) match {
      case NoNegativeOperands    =>
        if(isBigger(y, x) && operation == Subtract)
          Some("-" ++ Subtraction(y, x))
        else Some(executeComputation(x, y, operation))

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
      case Divide   => Division(x, y)
    }
  }

  private def handleBothOperandsNegative(x: String, y: String, operation: Operation): Option[String] = {
    operation match {
      case Add      =>
        Some("-" ++ Addition(x, y))
      case Subtract =>
        if(isBigger(x, y)) Some("-" ++ Subtraction(x, y))
        else      Some(Subtraction(y, x))
      case Multiply =>
        Some(Multiplication(x, y))
      case Divide   =>
        if ( isDivisorZero(y))
          None
        else
          Some(Division(x, y))
      case Pow      =>
        None
    }
  }

  private def isDivisorZero(x: String): Boolean = x.dropWhile(_.equals('0')).isEmpty

  private def handleNegativeLeftOperand(x: String, y: String, operation: Operation): Option[String] = {
    operation match {
      case Add      =>
        if (isBigger(x, y)) Some("-" ++ Subtraction(x, y))
        else Some(Subtraction(y, x))
      case Subtract => Some("-" ++ Addition(x, y))
      case Multiply => Some("-" ++ Multiplication(x, y))
      case Divide   =>
        if (isDivisorZero(y))
          None
        else
          Some("-" ++ Division(x, y))
      case Pow      => None
    }
  }

  private def handleNegativeRightOperand(x: String, y: String, operation: Operation): Option[String] = {
    operation match {
      case Add      =>
        if(isBigger(x, y)) Some(Subtraction(x, y))
        else      Some("-" ++ Subtraction(y, x))
      case Subtract => Some(Addition(x, y))
      case Multiply => Some("-" ++ Multiplication(x, y))
      case Divide   =>
        if(isDivisorZero(y))
          None
        else
          Some("-" ++ Division(x, y))
      case Pow      => None
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
    x.filter(!_.equals('-')).forall(_.isDigit) &&
      y.filter(!_.equals('-')).forall(_.isDigit)
  }

  private def isSignumCorrect(x: String, operation: Operation): Boolean = {
    x.forall(_.isDigit) ||
      (
        x.startsWith("-") &&
        (x.count(_.equals('-')) == 1) &&
          operation != Pow
        )
  }

  private def areDefined(x: Option[String], y: Option[String]): Boolean = {
    x.isDefined && y.isDefined
  }


  private def isBigger(x: String, y: String): Boolean = {
    if(x.length > y.length)
      true
    else if(x > y) true
    else false
  }

  /*
    There are 3 levels of validation:
      1. both operands are defined
      2. both operands contain digits only, disregarding "-"
      3. the "-" operand is currently placed ( first position )
   */
  private def isValid(x: Option[String], y: Option[String], op: Operation): Boolean = {
    (x, y) match {
      case (None, None)       => false
      case (None, _)          => false
      case (_, None)          => false
      case (Some(a), Some(b)) =>
        if(isDigitsOnly(a, b)) {
          if (isSignumCorrect(a, op) && isSignumCorrect(b, op))
            true
          else
            false
        }
        else
          false
    }
  }
}
