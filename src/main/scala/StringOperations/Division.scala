package StringOperations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Division {
  private[StringOperations] def apply(x: String, y: String) = {
    @tailrec
    def divide(x: String, y: String, quotient: String ): String = {
      val x1 = Subtraction(x, y)

      if(x1 < y)
        Addition(quotient, "1")
      else
        divide(x1, y, Addition(quotient, "1"))
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
