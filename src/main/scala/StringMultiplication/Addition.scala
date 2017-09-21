package StringMultiplication

/**
  * Created by Robert-PC on 9/21/2017.
  */
case class Total(sum: String = "", carry: Int = 0)

object Addition {
  def apply(x: String, y: String): String = {
    val resultWithoutCarry = equalize(x, y).zip(equalize(y, x))
      .map(e => (
        getCarry(e._1, e._2),
        getCurrent(e._1, e._2)
      ))
      .foldRight(new Total())((current, total) =>
        Total(
          ((current._2 + total.carry) % 10).toString ++ total.sum,
          (current._2 + total.carry) / 10 + current._1
        )
      )

    if (resultWithoutCarry.carry > 0)
      resultWithoutCarry.carry.toString ++ resultWithoutCarry.sum
    else
      resultWithoutCarry.sum
  }

  private def getCurrent(x: Char, y: Char): Int = {
    (x.asDigit + y.asDigit) % 10
  }

  private def getCarry(x: Char, y: Char): Int = {
    (x.asDigit + y.asDigit) / 10
  }

  private def equalize(first: String, second: String): String = {
    if (first.length >= second.length)
      first
    else
      "0" * (second.length - first.length) ++ first
  }
}
