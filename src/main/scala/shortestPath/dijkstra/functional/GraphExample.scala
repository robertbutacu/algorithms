package shortestPath.dijkstra.functional

import shortestPath.dijkstra.functional.DijkstraFunc.{Distance, Edge, Graph, Node}

trait GraphExample extends NodesExample{
  /*
  val bacau = Node(1, "Bacau")
  val roman = Node(2, "Roman")
  val piatraNeamt = Node(3, "Piatra Neamt")
  val iasi = Node(4, "Iasi")
  val buzau = Node(5, "Buzau")
  val bucuresti = Node(6, "Bucuresti")
  val cluj = Node(7, "Cluj")
  val timisoara = Node(8, "Timisoara")
   */
  def getImmDijGraph: Graph = {
    val graph = Map[Edge, Distance](
      Edge(1, 2) -> 50,
      Edge(1, 3) -> 60,
      Edge(2, 4) -> 50,
      Edge(3, 4) -> 70,
      Edge(1, 5) -> 250,
      Edge(5, 6) -> 300,
      Edge(4, 1) -> 150,
      Edge(6, 7) -> 400,
      Edge(7, 6) -> 400,
      Edge(7, 8) -> 350,
      Edge(5, 7) -> 300
    )

    graph
  }

  def getNodes: List[Node] = {
    List(bacau, roman, piatraNeamt, iasi, cluj, bucuresti, buzau, timisoara)
  }
}
