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

  def compute(operation: Operation, x: Option[Number]): Option[Number] = {
    operation match {
      case _ => None
    }
  }

  private def handleComputation(x: Number, y: Number, operation: Operation): Option[Number] = {
    operation match {
      case Add      => Some(handleAddition(x, y))
      case Subtract => Some(handleSubtraction(x, y))
      case Multiply => Some(handleMultiplication(x, y))
      case Divide   => handleDivision(x, y)
      case Modulus  => Some(handleModulus(x, y))
      case Pow      => handlePow(x, y)
      case _        => None
    }
  }

  private def handleAddition(x: Number, y: Number): Number = {
    getSignums(x, y) match {
      case NoNegativeOperands   =>
        Pos(Addi(x(), y()))
      case NegativeLeftOperand  =>
        if (isBigger(x, y)) Neg(Sub(x(), y()))
        else Pos(Sub(y(), x()))
      case NegativeRightOperand =>
        if(isBigger(x, y)) Pos(Sub(x(), y()))
        else               Neg(Sub(y(), x()))
      case BothOperandsNegative =>
        Neg(Addi(x(), y()))
    }
  }

  private def handleMultiplication(x: Number, y: Number): Number = {
    getSignums(x, y) match {
      case NoNegativeOperands   => Pos(Mul(x(), y()))
      case NegativeRightOperand => Neg(Mul(x(), y()))
      case NegativeLeftOperand  => Neg(Mul(x(), y()))
      case BothOperandsNegative => Pos(Mul(x(), y()))
    }
  }

  private def handleSubtraction(x: Number, y: Number): Number = {
    getSignums(x, y) match {
      case NoNegativeOperands   =>
        if(isBigger(y, x)) Neg(Sub(y(), x()))
        else               Pos(Sub(x(), y()))
      case NegativeLeftOperand  => Neg(Addi(x(), y()))
      case NegativeRightOperand => Pos(Addi(x(), y()))
      case BothOperandsNegative =>
        if(isBigger(x, y)) Neg(Sub(x(), y()))
        else               Pos(Sub(y(), x()))
    }
  }

  private def handleDivision(x: Number, y: Number): Option[Number] = {
    if(isDivisorZero(y))
      None
    else
      getSignums(x, y) match {
        case NegativeLeftOperand  => Some(Neg(Div(x(), y())))
        case NegativeRightOperand => Some(Neg(Div(x(), y())))
        case _                    => Some(Pos(Div(x(), y())))
    }
  }

  private def handleModulus(x: Number, y: Number): Number = {
    Pos("0")
  }

  private def handlePow(x: Number, y: Number): Option[Number] = {
    getSignums(x, y) match {
      case NoNegativeOperands   => Some(Pos(Exp(x(), y())))
      case NegativeLeftOperand  => None
      case NegativeRightOperand => None
      case BothOperandsNegative => None
    }
  }

  private def getSignums(x: Number, y: Number): Signum = {
    (x, y) match {
      case (Neg(_), Neg(_)) => BothOperandsNegative
      case (Neg(_), Pos(_)) => NegativeLeftOperand
      case (Pos(_), Neg(_)) => NegativeRightOperand
      case (Pos(_), Pos(_)) => NoNegativeOperands
    }
  }

  private def isDivisorZero(x: Number): Boolean = x().dropWhile(_.equals('0')).isEmpty

  private def isDigitsOnly(x: Number, y: Number): Boolean = {
    x().filter(!_.equals('-')).forall(_.isDigit) &&
      y().filter(!_.equals('-')).forall(_.isDigit)
  }

  /*
   x is bigger than y in 2 cases:
      1. longer size
      2. same size, but, character for character, x is the first one to contain a bigger one.
   */

  private def isBigger(x: Number, y: Number): Boolean = {
    if (x().length > y().length || (x().length == y().length && x() > y()))
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
