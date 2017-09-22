package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */

object Multiplication {
  def apply(x: String, y: String): String = {
    x.map(digit =>
      multiplyByDigit(y, digit)
    )
      .foldRight("0")((curr, acc) =>
        Addition(if (acc == "0") curr else curr ++ "0", acc)
      )
  }

  private def multiplyByDigit(x: String, y: Char): String = {
    val productWithoutCarry =
      x.foldRight(Total())((curr, acc) =>
        Total(
          updateDigit(curr, y, acc.carry) ++ acc.total,
          updateCarry(curr, y, acc.carry)
        )
      )
    if (productWithoutCarry.carry > 0)
      productWithoutCarry.carry.toString ++ productWithoutCarry.total
    else
      productWithoutCarry.total
  }

  private def updateDigit(x: Char, y: Char, previousCarry: Int): String = ((x.asDigit * y.asDigit + previousCarry) % 10).toString

  private def updateCarry(x: Char, y: Char, previousCarry: Int): Int = ((x.asDigit * y.asDigit) + previousCarry) / 10
}
