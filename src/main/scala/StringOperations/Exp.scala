package StringOperations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/22/2017.
  */

object Exp extends OperationFactory{
  private[StringOperations] def apply(x: String, y: String): String = {
    @tailrec
    def computeExponentiation(x: String, y: String, product: String): String = {
      y match {
        case "0" => "1"
        case "1" => Mul(x, product)
        case _ => computeExponentiation(
          x,
          Sub(y, "1"),
          Mul(x, product)
        )
      }
    }

    computeExponentiation(x, y, "1")
  }
}
