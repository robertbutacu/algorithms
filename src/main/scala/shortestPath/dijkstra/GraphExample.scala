package shortestPath.dijkstra

trait GraphExample {
  var bacau = new Node("bacau")
  var iasi = new Node("iasi")
  var roman = new Node("roman")
  var piatraNeamt = new Node("piatra neamt")
  var bucuresti = new Node("bucuresti")
  var brasov = new Node("brasov")
  var cluj = new Node("cluj")
  var timisoara = new Node("timisoara")


  def populate(): Unit = {
    bacau.addNeighbors(Set((roman, 50), (piatraNeamt, 60)))
    roman.addNeighbors(Set((bacau, 50), (iasi, 100)))
    piatraNeamt.addNeighbors(Set((iasi, 110), (bacau, 60)))
    iasi.addNeighbors(Set((roman, 100), (piatraNeamt, 110)))
    bucuresti.addNeighbors(Set((bacau, 300)))
    brasov.addNeighbors(Set((cluj, 150), (timisoara, 250), (iasi, 400)))
    cluj.addNeighbors(Set((bucuresti, 400), (brasov, 150), (iasi, 200)))
    timisoara.addNeighbors(Set((brasov, 400), (cluj, 200), (iasi, 500)))

    val graph = List(bacau, iasi, roman, piatraNeamt, bucuresti, brasov, cluj, timisoara)
    graph.foreach(n => println( n.name + " " + n.neighbors.foreach(e => e._1.name + " " + e._2)))
  }

}
