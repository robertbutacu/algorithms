package stringOperations.operations

import stringOperations.{OperationFactory}
import stringOperations.utils.{Pos, Square, StringNumber}

/**
  * Created by Robert-PC on 9/25/2017.
  */
object Sqrt extends OperationFactory{
  def apply(x: String) = {
    Pos("1") to Pos(x) find ((i: StringNumber) => compute(Square, Some(i)) == x)
  }

}
