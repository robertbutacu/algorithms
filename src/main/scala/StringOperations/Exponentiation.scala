package StringOperations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/22/2017.
  */
object Exponentiation extends OperationFactory{
  private[StringOperations] def apply(x: String, y: String): String = {
    @tailrec
    def computeExponentiation(x: String, y: String, product: String): String = {
      y match {
        case "0" => "1"
        case "1" => Multiplication(x, product)
        case _ => computeExponentiation(
          x,
          Subtraction(y, "1"),
          Multiplication(x, product)
        )
      }
    }

    computeExponentiation(x, y, "1")
  }
}
