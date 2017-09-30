package stringOperations

import stringOperations.operations._
import stringOperations.utils._

/**
  * Created by Robert-PC on 9/25/2017.
  */
trait Handler{
  private[stringOperations] def handleComputation(x: StringNumber, y: StringNumber, operation: Operation): Option[StringNumber] = {
    operation match {
      case Add      => add(x, y)
      case Subtract => subtract(x, y)
      case Multiply => multiply(x, y)
      case Divide   => divide(x, y)
      case Modulus  => mod(x, y)
      case Pow      => pow(x, y)
      case _        => None
    }
  }

  private[stringOperations] def handleComputation(x: StringNumber, op: Operation): Option[StringNumber] = {
    op match {
      case Increment  => None
      case Decrement  => None
      case Root       => None
      case Square     => None
      case _          => None
    }
  }

  def add(x: StringNumber, y: StringNumber): Option[StringNumber] = {
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

  def multiply(x: StringNumber, y: StringNumber): Option[StringNumber] = {
    getSigns(x, y) match {
      case NoNegativeOperands   => Some(Pos(Mul(x(), y())))
      case NegativeRightOperand => Some(Neg(Mul(x(), y())))
      case NegativeLeftOperand  => Some(Neg(Mul(x(), y())))
      case BothOperandsNegative => Some(Pos(Mul(x(), y())))
      case InvalidOperation     => None
    }
  }

  def subtract(x: StringNumber, y: StringNumber): Option[StringNumber] = {
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

  def divide(x: StringNumber, y: StringNumber): Option[StringNumber] = {
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

  def mod(x: StringNumber, y: StringNumber): Option[StringNumber] = {
    if(isDivisorZero(y))
      None
    else
      getSigns(x, y) match {
        case NegativeLeftOperand  => Some(Neg(Mod(x(), y())))
        case NegativeRightOperand => Some(Neg(Mod(x(), y())))
        case InvalidOperation     => None
        case _                    => Some(Pos(Mod(x(), y())))
      }
  }

  def pow(x: StringNumber, y: StringNumber): Option[StringNumber] = {
    getSigns(x, y) match {
      case NoNegativeOperands   => Some(Pos(Exp(x(), y())))
      case NegativeLeftOperand  => None
      case NegativeRightOperand => None
      case BothOperandsNegative => None
      case InvalidOperation     => None
    }
  }

  private def getSigns(x: StringNumber, y: StringNumber): Sign = {
    (x, y) match {
      case (Neg(_), Neg(_)) => BothOperandsNegative
      case (Neg(_), Pos(_)) => NegativeLeftOperand
      case (Pos(_), Neg(_)) => NegativeRightOperand
      case (Pos(_), Pos(_)) => NoNegativeOperands
      case (_, _)           => InvalidOperation
    }
  }

  private def isDivisorZero(x: StringNumber): Boolean = x().dropWhile(_.equals('0')).isEmpty

  /*
  x is bigger than y in 2 cases:
    1. longer size
    2. same size, but, character for character, x is the first one to contain a bigger one.
 */

  private def isBigger(x: StringNumber, y: StringNumber): Boolean = {
    if (x().length > y().length || (x().length == y().length && x() > y()))
      true
    else
      false
  }
}
