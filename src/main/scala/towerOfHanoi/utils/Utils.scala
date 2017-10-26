package towerOfHanoi.utils

object Utils {
  /**
    * A hanoi tower problem has been solved if all pieces are on the final tower.
    * *
    * A game is represented as a list where :
    *1. the first element represents the number of pegs
    *2. the rest of the list represents the pieces, and their respective value the peg they sit on.
    **/

  def isFinalState(state: List[Int]): Boolean = state.forall(_ == state.head)

  def isInitialState(state: List[Int]): Boolean = state.slice(1, state.length).forall(_ == 1)

  /**
    * The game is initialized with a list where the first element represents the number of pegs,
    * and the rest of the list - the pieces that are by default positioned on the first peg.
    **/

  def initialize(numberOfPegs: Int, numberOfPieces: Int): List[Int] =
    List(numberOfPegs) ::: List.fill(numberOfPieces)(1)

  def transition(state: List[Int], piece: Int, peg: Int): List[Int] =
    state.updated(piece, peg)

  def isValid(state: List[Int], pieceIndex: Int, newPeg: Int): Boolean =
  // don't sit on itself, it's already there
    state(pieceIndex) != newPeg &&
      // no piece should be on top of the piece wanted to be moved
      // aka all pieces should be on a different peg up to the current piece
      !state.slice(1, pieceIndex).contains(state(pieceIndex)) &&
      //there isn't another piece of lower weight already on the new peg
      !state.slice(1, pieceIndex).contains(newPeg)
}
