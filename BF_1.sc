import $file.Graph, Graph._

class BF(graph: Graph) {
  case class Test(weight: Double, node: Option[String] = None)
  type Distances = scala.collection.mutable.ArrayBuffer[Double]
  type Path = scala.collection.mutable.ArrayBuffer[Int]
  val initialValue = Double.MaxValue

  def arbitrage = {
    val distances: Distances = initializeSingleSource(initialValue)
    val pre: Path = Array.fill(graph.vertexes.length)(-1).to(scala.collection.mutable.ArrayBuffer)
    relax(distances, pre)
    detectNegativeCycle(distances, pre)
  }

  private def initializeSingleSource(init: Double) = {
    val arr = Array.fill(graph.vertexes.length)(init).to(scala.collection.mutable.ArrayBuffer)
    arr(0) = 0
    arr
  }
  private def relax(distances: Distances, pre: Path) = {
    val n = graph.vertexes.size
    // Relax V-1 times
    for (_ <- 0 until n - 1) {
      for (from <- 0 until n) {
        for (to <- 0 until n) {
          if (distances(to) > distances(from) + graph.edges(from)(to).weight) {
            distances(to) = distances(from) + graph.edges(from)(to).weight
            pre(to) = from
          }
        }
      }
    }
  }

  private def detectNegativeCycle(distances: Distances, pre: Path) = {
    val n = graph.vertexes.size
    for (from <- 0 until n) {
      for (to <- 0 until n) {
        if (distances(to) > distances(from) + graph.edges(from)(to).weight) {
          println(s"Negative cycle found")
          val cycle = scala.collection.mutable.ArrayBuffer[Int](to, from)
          var x = from
          while (!cycle.contains(pre(from))) {
            cycle.append(pre(from))
            x = pre(from)
          }
          cycle.append(pre(x))
          println(cycle.reverse.map(i => graph.vertexes(i)).mkString(" -> "))
        }
      }
    }
  }

}
