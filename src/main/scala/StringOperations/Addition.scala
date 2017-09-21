package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
case class Total(sum: String = "", carry: Int = 0)

object Addition {
  def apply(x: String, y: String): String = {
    val resultWithoutCarry =
      equalizeLength(x, y)
        .zip(equalizeLength(y, x))
        .foldRight(Total())((curr, acc) =>
          Total(
            updateSum(curr, acc),
            updateCarry(curr, acc)
          )
        )

    if (resultWithoutCarry.carry > 0)
      resultWithoutCarry.carry.toString ++ resultWithoutCarry.sum
    else
      resultWithoutCarry.sum
  }

  // % 10 so it doesn't overflow - only need 1 digit
  private def updateSum(curr: (Char, Char), acc: Total): String = ((getCurrent(curr) + acc.carry) % 10).toString ++ acc.sum

  // division by 10 so it only keep the carry
  private def updateCarry(curr: (Char, Char), acc: Total): Int = (getCarry(curr) + acc.carry) / 10 + getCarry(curr)

  // no carry
  private def getCurrent(x: (Char, Char)): Int = {
    (x._1.asDigit + x._2.asDigit) % 10
  }

  //carry only
  private def getCarry(x: (Char, Char)): Int = {
    (x._1.asDigit + x._2.asDigit) / 10
  }

  private def equalizeLength(first: String, second: String): String = {
    if (first.length >= second.length)
      first
    else
      "0" * (second.length - first.length) ++ first
  }
}
