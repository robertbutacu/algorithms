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

  def toBits(input: String): List[Char] = {
    generatePowersOf2("1", input, List("1")).foldRight("", input){(curr, acc) =>
      println(curr + " " + acc._1 + " " + acc._2)
      if(isBigger(Sub(acc._2, curr), acc._2)) (acc._1 ++ "0", acc._2)
      else (acc._1 ++ "1", Sub(acc._2, curr))
    }._1.toList
  }

  def computePowersRightToLeft(base: String, power: List[Char]): List[Int] = {
    List()
  }



  private def isBigger(x: String, y: String): Boolean = {
    if (x.length > y.length || (x.length == y.length && x > y)) true
    else false
  }
}
