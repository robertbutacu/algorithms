package shortestPath.dijkstra.functional

import java.util.UUID

import scala.annotation.tailrec

object DijkstraFunc {
  type CityName      = String
  type Distance      = Int
  type NodeId        = Int
  type Graph         = Map[Edge, Distance]
  type Path          = (Nodes, Distance)
  type VisitedNodes  = Nodes
  type PriorityQueue = Nodes
  type Nodes         = List[Node]
  case class Node(id: NodeId,
                  name: CityName,
                  tentativeDistance : Distance = Int.MaxValue,
                  previous: Option[Node] = None)
  case class Edge(from: NodeId, to: NodeId)

  def shortest(from: Node, to: Node, nodes: Nodes, graph: Graph): Path = {
    //@tailrec
    def go(curr: Node, nodes: Nodes, pq: PriorityQueue, vn: VisitedNodes): Path = {
      /**
        * add curr to visited list
        *
        * updated current node's neighbors
        *
        * update priority queue
        *
        * recursive call to go until curr is equal to to
        */
      if(curr.name == to.name)
        (List(), curr.tentativeDistance)
      else{
        //println("Currently in " + curr.name)

        val neighbors = getNeighbors(curr, graph)

        val updatedVn = vn :+ curr

        val updatedNodes = transformNeighbors(curr, neighbors, nodes)

        val updatedPq = transformPriorityQueue(nodes.filter(n => neighbors.keys.toList.contains(n.id)), updatedVn, pq filterNot(_.name == curr.name))

        updatedNodes.foreach(n => println(n.name + " " + n.tentativeDistance))

        println()
        go(updatedPq.head,
          updatedNodes,
          updatedPq,
          updatedVn)
      }
    }

    go(from, nodes, List(), List())
  }

  def transformNeighbors(curr: Node, neighbors: Map[NodeId, Distance], nodes: Nodes): Nodes = {
    nodes.map(n =>
      if (neighbors.keys.toList.contains(n.id) && (neighbors(n.id) + curr.tentativeDistance < n.tentativeDistance)){
        //println("In " + curr.name + " to " + n.name + " adding " + curr.tentativeDistance + " " + neighbors(n.id))
        Node(n.id, n.name, neighbors(n.id) + curr.tentativeDistance, Some(curr))
      }
        else
        n
    )
  }

  def getNeighbors(node: Node, graph: Graph): Map[NodeId, Distance] = {
    graph filterKeys(e => e.from == node.id) map(c => (c._1.to, c._2))
  }

  def orderPriorityQueue(priorityQueue: PriorityQueue): PriorityQueue = {
    priorityQueue sortWith (_.tentativeDistance < _.tentativeDistance)
  }

  def transformPriorityQueue(nodes: List[Node], visitedNodes: VisitedNodes, priorityQueue: PriorityQueue): PriorityQueue = {
    orderPriorityQueue(priorityQueue ++ (nodes filterNot visitedNodes.contains filterNot priorityQueue.contains))
  }
}
