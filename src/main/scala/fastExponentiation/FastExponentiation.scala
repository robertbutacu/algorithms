package fastExponentiation

/**
  * Created by Robert-PC on 9/21/2017.
  */
object FastExponentiation {

  def isPowerOf2(input: Int): Boolean = {
    powers.takeWhile(_ * 2 <= input).exists(_ * 2 == input)
  }

  lazy val powers: Stream[Int] = Stream.cons(1,Stream.from(2, 2).filter(isPowerOf2))

}
