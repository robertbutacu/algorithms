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

  def computePowersRightToLeft(base: Int, power: List[Char]): List[Int] = {
    power
      .scanRight(0)((_, acc) =>
        if (acc == 0) base
        else acc * acc
      )
  }

  def pow(base: Int, power: Int): Int = {
    computePowersRightToLeft(base, toBits(power)).zip(toBits(power))
      .foldRight(1)((curr, acc) => {
        println(curr + " " + acc)

        if (curr._2.asDigit == 1) curr._1 * acc
        else acc
      })
  }
}
