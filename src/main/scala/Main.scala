import fastExponentiation.FastExponentiation
import greedyProblems.huffmanCoding.Huffman
import stringOperations._
import stringOperations.examples.StreamsExamples
import stringOperations.operations.{FastExp, Mod}
import stringOperations.utils._

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Main extends App with OperationFactory with StreamsExamples{
  println(FastExp("2", "1000"))
}
