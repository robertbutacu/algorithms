package shortestPath.utils

trait BellmanFordGraph extends NodesExample with Utils {
  def getBFGraph: Graph = {
    bacau.addNeighbors(List((roman, 8), (piatraNeamt, 10)))
    roman.addNeighbors(List((buzau, 1)))
    piatraNeamt.addNeighbors(List((cluj, 2)))
    buzau.addNeighbors(List((piatraNeamt, -4), (cluj, -1)))
    cluj.addNeighbors(List((bucuresti, -2)))
    bucuresti.addNeighbors(List((piatraNeamt, 1)))

    val graph: Graph = List(bacau, roman, piatraNeamt, iasi, bucuresti, brasov, cluj, timisoara, buzau)
    graph
  }

}
