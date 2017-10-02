package shortestPath.dijkstra

import shortestPath.dijkstra.Dijkstra.Edge

trait GraphExample {
  val bacau = Dijkstra.Node("Bacau", Some(50))
  val iasi = Dijkstra.Node("Iasi")
  val roman = Dijkstra.Node("Roman")
  val piatraNeamt = Dijkstra.Node("Piatra-Neamt")
  val bucuresti = Dijkstra.Node("Bucuresti")
  val brasov = Dijkstra.Node("Brasov")
  val cluj = Dijkstra.Node("Cluj")
  val timisoara = Dijkstra.Node("Timisoara")

  val graph = Dijkstra.Graph(List(
    Edge((bacau, roman), 60),
    Edge((bacau, piatraNeamt), 70),
    Edge((roman, iasi), 70),
    Edge((piatraNeamt, iasi), 80),
    Edge((piatraNeamt, brasov), 400),
    Edge((piatraNeamt, cluj), 450),
    Edge((cluj, brasov), 100),
    Edge((cluj, timisoara), 300),
    Edge((brasov, timisoara), 250),
    Edge((brasov, bucuresti), 450),
    Edge((bacau, bucuresti), 300)
  ))
}
