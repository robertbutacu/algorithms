package StringOperations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/22/2017.
  */
object Exponentiation extends OperationFactory{
  def apply(x: String, y: String): String = {
      computeExponentiation(x, y, "1")
  }

  private def computeExponentiation(x: String, y: String, product: String): String = {
    @tailrec
    def computeForCurrent(x: String, y: String, product: String): String = {
      y match {
        case "0" => "1"
        case "1" => Multiplication(x, product)
        case _ => computeForCurrent(
          x,
          Subtraction(y, "1"),
          Multiplication(x, product)
        )
      }
    }

    computeForCurrent(x, y, product)
  }

  private def isValid(x: String, y: String): Boolean = x.forall(_.isDigit) && y.forall(_.isDigit)
}
