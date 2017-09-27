package greedyProblems.huffmanCoding

/**
  * Created by Robert-PC on 9/27/2017.
  */
abstract class Tree

case class LeafNode(char: Char, freq: Int)                      extends Tree
case class Fork(left: Tree, chars: List[Char], freq: Int, right: Tree) extends Tree
