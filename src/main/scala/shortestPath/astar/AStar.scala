package shortestPath.astar

import scala.annotation.tailrec
import towerOfHanoi.utils.Utils._

object AStar {
  def solveTowerOfHanoi(pegs: Int, pieces: Int): (Int, List[Node]) = {
    val nodes = getGraph(initialize(pegs, pieces), pieces, pegs)

    updateDistancesToFinal(nodes)

    getShortestDistance(nodes)
  }

  def getGraph(initialState: List[Int], pieces: Int, pegs: Int): List[Node] = {
    @tailrec
    def go(currentNode: Node, queue: List[Node], allNodes: List[Node]): List[Node] = {
      if (queue.isEmpty)
        allNodes :+ currentNode
      else {
        //generate all possible transitions from current state
        val newStates = for {
          allPieces <- (1 to pieces).toStream
          allPegs <- (1 to pegs).toStream
          if isValid(currentNode.state, allPieces, allPegs)
        } yield transition(currentNode.state, allPieces, allPegs)


        //creating new neighbours to the current node
        val newNodes = newStates.toList
          .map(e => new Node(e, Some(currentNode)))

        currentNode.neighbors = currentNode.neighbors ::: newNodes

        //filtering out already nodes that were created to not go through cycles
        val reachableNodes = newNodes.filterNot(n => allNodes.exists(an => an.state == n.state ||
          an.previousState.map(e => e.state) == n.previousState.map(e => e.state)))

        go(queue.head, queue.tail ::: reachableNodes, allNodes :+ currentNode)
      }
    }

    val initialNode = new Node(initialState, None)
    go(initialNode, List(initialNode), List.empty)
  }

  def updateDistancesToFinal(input: List[Node]): Unit = {
    def go(node: Node, distanceToFinal: Int): Unit = {
      if (isInitialState(node.state)) {
        node.distanceToFinal = distanceToFinal + 1
      }
      else {
        node.previousState match {
          case Some(n) =>
            // either the distance to final node is updated for the current node
            if (n.distanceToFinal > distanceToFinal + 1) {
              n.distanceToFinal = distanceToFinal + 1
              go(n, distanceToFinal + 1)
            }
            // or the distance to final node of the current node is smaller, which replaces current dist
            else
              go(n, n.distanceToFinal)
          case None    =>
        }
      }

    }

    input filter (n => isFinalState(n.state)) foreach (_.distanceToFinal = 0)
    input filter (n => isFinalState(n.state)) foreach (go(_, 0))
  }

  def getShortestDistance(input: List[Node]): (Int, List[Node]) = {
    @tailrec
    def go(node: Node, path: List[Node]): (Int, List[Node]) = {
      if (isFinalState(node.state))
        (path.length, path)
      else {
        // sorting to get neighbor with smallest distance to final node
        node.neighbors = node.neighbors.sortWith((n1, n2) => n1.distanceToFinal <= n2.distanceToFinal)
        go(node.neighbors.head, path :+ node.neighbors.head)
      }
    }

    val initialNodes = input.filter(n => isInitialState(n.state))
    initialNodes.sortWith((n1, n2) => n1.distanceToFinal <= n2.distanceToFinal)

    go(initialNodes.head, List.empty)
  }
}
