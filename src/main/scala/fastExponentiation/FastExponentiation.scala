package fastExponentiation

/**
  * Created by Robert-PC on 9/21/2017.
  */
object FastExponentiation {

  def isPowerOf2(input: Int): Boolean = {
    powers.takeWhile(_ * 2 <= input).exists(_ * 2 == input)
  }

  lazy val powers: Stream[Int] = Stream.cons(1,Stream.from(2, 2).filter(isPowerOf2))

  def toBits(input: Int): List[Char] = {
    powers.takeWhile(_ <= input)
      .reverse
      .toList
      .foldLeft("", input)((acc, curr) =>
      if(acc._2 - curr >= 0) (acc._1 ++ "1", acc._2 - curr)
      else                   (acc._1 ++ "0", acc._2)
    )
      ._1
      .toList
  }
}
