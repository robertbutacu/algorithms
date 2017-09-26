package stringOperations.operations

import stringOperations.utils.StringNumber

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/25/2017.
  */
object Mod {
  def apply(x: String, y: String): String = {
    @tailrec
    def mod(x: String, y: String): String = {
      val x1 = Sub(x, y)

      if(x1 < y)
        x1
      else
        mod(x1, y)
    }

    if(y.equals("1"))
      x
    else
    if(x.length < y.length)
      "0"
    else
      mod(x, y)
  }
}
