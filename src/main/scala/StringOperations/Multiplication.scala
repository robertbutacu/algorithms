package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */

object Multiplication {
  def apply(x: String, y: String): String = {
    x.map(digit =>
      multiplyByDigit(y, digit)
    )
      .foldRight("0")((curr, acc ) =>
        Addition(curr, acc)
      )
  }

  private def multiplyByDigit(x: String, y: Char): String = {
    "0"
  }
}
