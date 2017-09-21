package StringMultiplication

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Addition {
  def apply(x: String, y: String): String = {
    val resultWithoutCarry = x.zip(y)
      .map(pair => (
        (pair._1.asDigit + pair._2.asDigit) / 10, //carry
        (pair._1.asDigit + pair._2.asDigit) % 10
      ))
      .foldRight(("", 0))((current, total) =>
        (((current._2 + total._2) % 10).toString ++ total._1,
          (current._2 + total._2) / 10 + current._1
        )
      )
    println(resultWithoutCarry._1 + " carry " + resultWithoutCarry._2)

    if(resultWithoutCarry._2 > 0)
      resultWithoutCarry._2.toString ++ resultWithoutCarry._1
    else
      resultWithoutCarry._1
  }

  def longerString(first: String, second: String): String = {
    if(first.length >= second.length)
      first
    else
      second
  }

  def shorterString(first: String, second: String): String = {
    if(second.length >= first.length)
      second
    else
      first
  }
}
