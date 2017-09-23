package StringOperations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/22/2017.
  */
object Exponentiation {
  def apply(x: String, y: String): Option[String] = {
    if(isValid(x, y))
      compute(x, Some(y), Some("1"))
    else
      None
  }

  private def compute(x: String, y: Option[String], product: Option[String]): Option[String] = {
    @tailrec
    def computeForCurrent(x: String, y: Option[String], product: Option[String]): Option[String] = {
      y match {
        case None => None
        case Some("0") => Some("1")
        case Some("1") => OperationFactory(x, product.get, Multiply)
        case _ => computeForCurrent(
          x,
          OperationFactory(y.get, "1", Subtract),
          OperationFactory(x, product.get, Multiply)
        )
      }
    }

    computeForCurrent(x, y, product)
  }

  private def isValid(x: String, y: String): Boolean = x.forall(_.isDigit) && y.forall(_.isDigit)
}
