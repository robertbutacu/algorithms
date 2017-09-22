package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Substraction {
  def apply(x: String, y: String): String = {
    Utils.equalizeLength(x, y).zip(Utils.equalizeLength(y, x))
      .foldRight(Total())((curr, acc) =>
        Total(
          subtract(curr._1, curr._2, acc.carry),
          carry(curr._1, curr._2, acc.carry)
        )
      ).total
  }

  private def subtract(x: Char, y: Char, carry: Int): String = (x.asDigit - y.asDigit + carry + 9).toString

  private def carry(x: Char, y: Char, carry: Int): Int = Math.max(x.asDigit - y.asDigit + carry, 0)

}
