package stringOperations

import stringOperations.utils.Operation

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait OperationFactory extends Handler{
  def compute(x: Option[utils.Number], operation: Operation, y: Option[utils.Number]): Option[utils.Number] = {
    isValid(x, y, operation) match {
      case Some((a, b)) => handleComputation(a, b, operation)
      case _            => None
    }
  }

  def compute(operation: Operation, x: Option[utils.Number]): Option[utils.Number] = {
    operation match {
      case _ => None
    }
  }

  private def isDigitsOnly(x: utils.Number, y: utils.Number): Boolean = {
    x().filter(!_.equals('-')).forall(_.isDigit) &&
      y().filter(!_.equals('-')).forall(_.isDigit)
  }

  /*
    There are 3 levels of validation:
      1. both operands are defined
      2. both operands contain digits only, disregarding "-"
      3. the "-" operand is currently placed ( first position )
   */

  private def isValid(x: Option[utils.Number], y: Option[utils.Number], op: Operation): Option[(utils.Number, utils.Number)] = {
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
