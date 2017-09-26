package stringOperations

import stringOperations.utils.{Operation, StringNumber}

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory extends Handler{
  def compute(x: Option[StringNumber], operation: Operation, y: Option[StringNumber]): Option[StringNumber] = {
    isValid(x, y, operation) match {
      case Some((a, b)) => handleComputation(a, b, operation)
      case _            => None
    }
  }

  def compute(operation: Operation, x: Option[StringNumber]): Option[StringNumber] = {
    operation match {
      case _ => None
    }
  }


  private def isDigitsOnly(x: StringNumber, y: StringNumber): Boolean = {
    x().filter(!_.equals('-')).forall(_.isDigit) &&
      y().filter(!_.equals('-')).forall(_.isDigit)
  }

  /*
    There are 3 levels of validation:
      1. both operands are defined
      2. both operands contain digits only, disregarding "-"
      3. the "-" operand is currently placed ( first position )
   */

  private def isValid(x: Option[StringNumber], y: Option[StringNumber], op: Operation): Option[(StringNumber, StringNumber)] = {
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
