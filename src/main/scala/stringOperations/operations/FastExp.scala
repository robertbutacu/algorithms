package stringOperations.operations

object FastExp {

  private[stringOperations] def apply(x: String, y: String): String = {
    ""
  }

  def increment(curr: String, goal: String, result: List[String]): List[String] = {
    if(Mul(result.last, "2") == curr) generatePowersOf2(Inc(curr), goal, result ::: List(curr))
    else generatePowersOf2(Inc(curr), goal, result)
  }

  def generatePowersOf2(start: String, goal: String, result: List[String]): List[String] = {
    if(isBigger(start, goal)) result
    else increment(start, goal, result)
  }

  def computePowersRightToLeft(base: String, power: List[Char]): List[Int] = {
    List()
  }

  def toBits(input: String): List[Char] = List()

  private def isBigger(x: String, y: String): Boolean = {
    if (x.length > y.length || (x.length == y.length && x > y)) true
    else false
  }
}
