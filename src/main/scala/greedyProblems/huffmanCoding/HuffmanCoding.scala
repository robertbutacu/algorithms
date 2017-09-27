package greedyProblems.huffmanCoding

/**
  * Created by Robert-PC on 9/21/2017.
  */
object HuffmanCoding {

  def getFrequency(input: String): scala.collection.mutable.Map[Char, Int] = {
    var frequency = scala.collection.mutable.Map[Char, Int]()

    input.foreach(c =>
    if(frequency.contains(c))
      frequency(c) = frequency.getOrElse(c, 0) + 1
    else
      frequency += (c -> 1)
    )

    frequency
  }

}
