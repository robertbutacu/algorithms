package shortestPath.utils

trait DijkstraGraph extends NodesExample with Utils {
  def getGraph: Graph = {
    bacau.addNeighbors(List((roman, 50), (piatraNeamt, 60), (buzau, 300)))
    roman.addNeighbors(List((bacau, 50), (iasi, 100), (cluj, 300)))
    piatraNeamt.addNeighbors(List((iasi, 110), (bacau, 60), (brasov, 400)))
    iasi.addNeighbors(List((roman, 100), (piatraNeamt, 110)))
    bucuresti.addNeighbors(List((bacau, 300)))
    brasov.addNeighbors(List((cluj, 150), (timisoara, 250), (iasi, 400)))
    cluj.addNeighbors(List((bucuresti, 400), (brasov, 150), (iasi, 200)))
    timisoara.addNeighbors(List((brasov, 400), (cluj, 200), (iasi, 500)))
    buzau.addNeighbors(List((bacau, 250), (bucuresti, 300)))

    val graph = List(bacau, iasi, roman, piatraNeamt, bucuresti, brasov, cluj, timisoara, buzau)
    graph
  }
}
