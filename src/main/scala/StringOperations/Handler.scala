package StringOperations

/**
  * Created by Robert-PC on 9/25/2017.
  */
trait Handler {
  private[StringOperations] def handleComputation(x: Number, y: Number, operation: Operation): Option[Number] = {
    operation match {
      case Add      => handleAddition(x, y)
      case Subtract => handleSubtraction(x, y)
      case Multiply => handleMultiplication(x, y)
      case Divide   => handleDivision(x, y)
      case Modulus  => handleModulus(x, y)
      case Pow      => handlePow(x, y)
      case _        => None
    }
  }

  private def handleAddition(x: Number, y: Number): Option[Number] = {
    getSigns(x, y) match {
      case NoNegativeOperands   =>
        Some(Pos(Addi(x(), y())))
      case NegativeLeftOperand  =>
        if (isBigger(x, y)) Some(Neg(Sub(x(), y())))
        else                Some(Pos(Sub(y(), x())))
      case NegativeRightOperand =>
        if(isBigger(x, y)) Some(Pos(Sub(x(), y())))
        else               Some(Neg(Sub(y(), x())))
      case BothOperandsNegative =>
        Some(Neg(Addi(x(), y())))
      case InvalidOperation     =>
        None
    }
  }

  private def handleMultiplication(x: Number, y: Number): Option[Number] = {
    getSigns(x, y) match {
      case NoNegativeOperands   => Some(Pos(Mul(x(), y())))
      case NegativeRightOperand => Some(Neg(Mul(x(), y())))
      case NegativeLeftOperand  => Some(Neg(Mul(x(), y())))
      case BothOperandsNegative => Some(Pos(Mul(x(), y())))
      case InvalidOperation     => None
    }
  }

  private def handleSubtraction(x: Number, y: Number): Option[Number] = {
    getSigns(x, y) match {
      case NoNegativeOperands   =>
        if(isBigger(y, x)) Some(Neg(Sub(y(), x())))
        else               Some(Pos(Sub(x(), y())))
      case NegativeLeftOperand  =>
        Some(Neg(Addi(x(), y())))
      case NegativeRightOperand =>
        Some(Pos(Addi(x(), y())))
      case BothOperandsNegative =>
        if(isBigger(x, y)) Some(Neg(Sub(x(), y())))
        else               Some(Pos(Sub(y(), x())))
      case InvalidOperation     =>
        None
    }
  }

  private def handleDivision(x: Number, y: Number): Option[Number] = {
    if(isDivisorZero(y))
      None
    else
      getSigns(x, y) match {
        case NegativeLeftOperand  => Some(Neg(Div(x(), y())))
        case NegativeRightOperand => Some(Neg(Div(x(), y())))
        case InvalidOperation     => None
        case _                    => Some(Pos(Div(x(), y())))
      }
  }

  private def handleModulus(x: Number, y: Number): Option[Number] = {
    Some(Pos("0"))
  }

  private def handlePow(x: Number, y: Number): Option[Number] = {
    getSigns(x, y) match {
      case NoNegativeOperands   => Some(Pos(Exp(x(), y())))
      case NegativeLeftOperand  => None
      case NegativeRightOperand => None
      case BothOperandsNegative => None
      case InvalidOperation     => None
    }
  }

  private def getSigns(x: Number, y: Number): Sign = {
    (x, y) match {
      case (Neg(_), Neg(_)) => BothOperandsNegative
      case (Neg(_), Pos(_)) => NegativeLeftOperand
      case (Pos(_), Neg(_)) => NegativeRightOperand
      case (Pos(_), Pos(_)) => NoNegativeOperands
      case (_, _)           => InvalidOperation
    }
  }

  private def isDivisorZero(x: Number): Boolean = x().dropWhile(_.equals('0')).isEmpty

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
}
