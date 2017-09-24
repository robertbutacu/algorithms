package StringOperations

/**
  * Created by Robert-PC on 9/22/2017.
  */
case class Total(total: String = "", carry: Int = 0)

trait InputException

case class InvalidInputException(msg: String) extends InputException
case object Valid                             extends InputException

object Utils {
  def equalizeLength(first: String, second: String): String = {
    if (first.length >= second.length)
      first
    else
      "0" * (second.length - first.length) ++ first
  }
}
