package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory {
  def compute(x: Option[Number], operation: Operation, y: Option[Number]): Option[Number] = {
    isValid(x, y, operation) match {
      case Some((a, b)) => handleComputation(a, b, operation)
      case _            => None
    }

  }

  private def handleComputation(x: Number, y: Number, operation: Operation): Option[Number] = {
    getSignums(x, y) match {
      case NoNegativeOperands    =>
        if(isBigger(y, x) && operation == Subtract)
          Some(Negative(Subtraction(y.number, x.number)))
        else Some(Positive(executeComputation(x.number, y.number, operation)))

      case NegativeLeftOperand   =>
        handleNegativeLeftOperand(x, y, operation)

      case NegativeRightOperand  =>
        handleNegativeRightOperand(x, y, operation)

      case BothOperandsNegative  =>
        handleBothOperandsNegative(x, y, operation)
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

  private def handleBothOperandsNegative(x: Number, y: Number, operation: Operation): Option[Number] = {
    operation match {
      case Add      =>
        Some(Negative(Addition(x.number, y.number)))
      case Subtract =>
        if(isBigger(x, y)) Some(Negative(Subtraction(x.number, y.number)))
        else               Some(Positive(Subtraction(y.number, x.number)))
      case Multiply =>
        Some(Positive(Multiplication(x.number, y.number)))
      case Divide   =>
        if ( isDivisorZero(y))
          None
        else
          Some(Positive(Division(x.number, y.number)))
      case Pow      =>
        None
    }
  }

  private def isDivisorZero(x: Number): Boolean = x.number.dropWhile(_.equals('0')).isEmpty

  private def handleNegativeLeftOperand(x: Number, y: Number, operation: Operation): Option[Number] = {
    operation match {
      case Add      =>
        if (isBigger(x, y)) Some(Negative(Subtraction(x.number, y.number)))
        else Some(Positive(Subtraction(y.number, x.number)))
      case Subtract => Some(Negative(Addition(x.number, y.number)))
      case Multiply => Some(Negative(Multiplication(x.number, y.number)))
      case Divide   =>
        if (isDivisorZero(y))
          None
        else
          Some(Negative(Division(x.number, y.number)))
      case Pow      => None
    }
  }

  private def handleNegativeRightOperand(x: Number, y: Number, operation: Operation): Option[Number] = {
    operation match {
      case Add      =>
        if(isBigger(x, y)) Some(Positive(Subtraction(x.number, y.number)))
        else      Some(Negative(Subtraction(y.number, x.number)))
      case Subtract => Some(Positive(Addition(x.number, y.number)))
      case Multiply => Some(Negative(Multiplication(x.number, y.number)))
      case Divide   =>
        if(isDivisorZero(y))
          None
        else
          Some(Negative(Division(x.number, y.number)))
      case Pow      => None
    }
  }

  private def getSignums(x: Number, y: Number): Signum = {
    (x, y) match {
      case (Negative(_), Negative(_)) => BothOperandsNegative
      case (Negative(_), Positive(_)) => NegativeLeftOperand
      case (Positive(_), Negative(_)) => NegativeRightOperand
      case (Positive(_), Positive(_)) => NoNegativeOperands
    }
  }

  private def isDigitsOnly(x: Number, y: Number): Boolean = {
    x.number.filter(!_.equals('-')).forall(_.isDigit) &&
      y.number.filter(!_.equals('-')).forall(_.isDigit)
  }

  /*
   x is bigger than y in 2 cases:
      1. longer size
      2. same size, but, character for character, x is the first one to contain a bigger one.
   */

  private def isBigger(x: Number, y: Number): Boolean = {
    if (x.number.length > y.number.length || (x.number.length == y.number.length && x.number > y.number))
      true
    else
      false
  }

  /*
    There are 3 levels of validation:
      1. both operands are defined
      2. both operands contain digits only, disregarding "-"
      3. the "-" operand is currently placed ( first position )
   */
  private def isValid(x: Option[Number], y: Option[Number], op: Operation): Option[(Number, Number)] = {
    (x, y) match {
      case (Some(a), Some(b)) =>
        if(isDigitsOnly(a, b))
          Some((a, b))
        else
          None
      case (_, _)             =>
        None
    }
  }
}
