package shortestPath.utils

import shortestPath.dijkstra.Dijkstra.Graph

trait GraphExample {
  var bacau = new Node("bacau", List(), 0)
  var iasi = new Node("iasi")
  var roman = new Node("roman")
  var piatraNeamt = new Node("piatra neamt")
  var bucuresti = new Node("bucuresti")
  var brasov = new Node("brasov")
  var cluj = new Node("cluj")
  var timisoara = new Node("timisoara")


  def getGraph: Graph = {
    bacau.addNeighbors(List((roman, 50), (piatraNeamt, 60)))
    roman.addNeighbors(List((bacau, 50), (iasi, 100), (cluj, 300)))
    piatraNeamt.addNeighbors(List((iasi, 110), (bacau, 60), (brasov, 400)))
    iasi.addNeighbors(List((roman, 100), (piatraNeamt, 110)))
    bucuresti.addNeighbors(List((bacau, 300)))
    brasov.addNeighbors(List((cluj, 150), (timisoara, 250), (iasi, 400)))
    cluj.addNeighbors(List((bucuresti, 400), (brasov, 150), (iasi, 200)))
    timisoara.addNeighbors(List((brasov, 400), (cluj, 200), (iasi, 500)))

    val graph = List(bacau, iasi, roman, piatraNeamt, bucuresti, brasov, cluj, timisoara)
    graph
  }
}
