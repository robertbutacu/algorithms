package stringOperations.operations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */

object Div {
  private[stringOperations] def apply(x: String, y: String) = {
    @tailrec
    def divide(x: String, y: String, quotient: String ): String = {
      val x1 = Sub(x, y)

      if(x1 < y)
        Addi(quotient, "1")
      else
        divide(x1, y, Addi(quotient, "1"))
    }

    if(y.equals("1"))
      x
    else
      if(x.length < y.length)
        "0"
      else
        divide(x, y, "0")
  }
}
