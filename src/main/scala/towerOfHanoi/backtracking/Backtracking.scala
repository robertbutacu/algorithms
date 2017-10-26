package towerOfHanoi.backtracking


import towerOfHanoi.utils.{All, BacktrackingMethod, First}
import towerOfHanoi.utils.Utils._

import scala.annotation.tailrec

object Backtracking {
  type States = List[List[Int]]

  def solveHanoi(pegs: Int, pieces: Int, method: BacktrackingMethod): Either[Int, Unit] = {
    def all(currentState: List[Int], currentPieceIndex: Int,
            previousStates: States,
            road: States): Unit = {
      if (isFinalState(currentState)) {
        println("Road: " + road)
        println("Road: " + road.length)
      }
      else {
        //all possible routes that could be taken from the current piece index in current state
        val validStates = (1 to currentState.head).toList
          .filter(e => isValid(currentState, currentPieceIndex, e))

        //a list of all possible states, not including previous states
        val possibleStates = validStates
          .map(p => transition(currentState, currentPieceIndex, p))
          .filterNot(previousStates.contains(_))

        //for each possible state, recursively call the function for each piece apart from the current one
        possibleStates.foreach(s =>
          (1 to pieces).toList
            .filterNot(_ == currentPieceIndex)
            .foreach(piece => all(s, piece, previousStates :+ s, road :+ s)))
      }
    }

    @tailrec
    def first(states: List[List[Int]], visited: Set[List[Int]], road: States): Int = {
      if (isFinalState(states.head))
        (road :+ states.head).filterNot(_.isEmpty).length
      else {
        val currentState = states.head

        //generate all possible transitions from current state
        var newStates = for {
          allPieces <- (1 to pieces).toStream
          allPegs <- (1 to pegs).toStream
          if isValid(currentState, allPieces, allPegs)
        } yield transition(currentState, allPieces, allPegs)

        //filtering so cycles are not met
        newStates = newStates filterNot (s => states.contains(s) || visited.contains(s)) filterNot (_.isEmpty)

        first(states.drop(1) ++ newStates, visited + states.head, road :+ states.head)
      }
    }

    method match {
      case First => Left(first(List(initialize(pegs, pieces)), Set.empty, List(List.empty)))
      case All   => Right(all(initialize(pegs, pieces), 1, List(List.empty), List()))
    }
  }
}
