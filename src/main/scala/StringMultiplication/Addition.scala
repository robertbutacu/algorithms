package StringMultiplication

/**
  * Created by Robert-PC on 9/21/2017.
  */
case class Total(sum: String = "", carry: Int = 0)
/*
    The algorithm is rather simple:
    1. both strings become of same size by making the both strings of same size ( so there arent any losses) , using the neutral
        element. Ex: "1", "1000" => "0001", "1000"
    2. match every digit with its corresponding digit
    3. for every tuple, add the digits, saving the sum and the carry.
      Ex: (9,9) => (1, 8) => 1 is carry, 8 is sum.
    4. Folding from right to left the result -> the accumulator keeps track of the final sum and the carry.

 */


object Addition {
  def apply(x: String, y: String): String = {
    val resultWithoutCarry = equalizeLength(x, y).zip(equalizeLength(y, x))
      .map(e => (
        getCarry(e._1, e._2),
        getCurrent(e._1, e._2)
      ))
      .foldRight(Total())((curr, acc) =>
        Total(
          updateSum(curr._2, acc),
          updateCarry(curr, acc)
        )
      )

    if (resultWithoutCarry.carry > 0)
      resultWithoutCarry.carry.toString ++ resultWithoutCarry.sum
    else
      resultWithoutCarry.sum
  }

  private def updateSum(curr: Int, acc: Total): String = ((curr + acc.carry) % 10).toString ++ acc.sum

  private def updateCarry(curr: (Int, Int), acc: Total): Int = (curr._2 + acc.carry) / 10 + curr._1

  private def getCurrent(x: Char, y: Char): Int = {
    (x.asDigit + y.asDigit) % 10
  }

  private def getCarry(x: Char, y: Char): Int = {
    (x.asDigit + y.asDigit) / 10
  }

  private def equalizeLength(first: String, second: String): String = {
    if (first.length >= second.length)
      first
    else
      "0" * (second.length - first.length) ++ first
  }
}
