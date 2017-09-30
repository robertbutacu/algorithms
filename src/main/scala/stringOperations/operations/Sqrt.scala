package stringOperations.operations

import stringOperations.utils.{Pos, StringNumber}

/**
  * Created by Robert-PC on 9/25/2017.
  */
object Sqrt{
  private[stringOperations] def apply(x: String) = Pos("1") to Pos(x) find ((i: StringNumber) => Sq(i()) == x)
}
