import $ivy.`org.jgrapht:jgrapht-core:1.5.1`, org.jgrapht._
import $file.DataReader
import org.jgrapht.graph._
import org.jgrapht.alg.shortestpath.BellmanFordShortestPath
import org.jgrapht.alg.shortestpath.NegativeCycleDetectedException

val currencies = DataReader.readUnsafe("data/example.json")
val graph = createGraph(currencies)
val alg = new BellmanFordShortestPath(graph)
try {
  alg.getPaths("USD")
} catch {
  case e: NegativeCycleDetectedException =>
    println(e.getCycle().getEndVertex())
    println(e.getCycle().getVertexList())
    println(scala.math.exp(scala.math.abs(e.getCycle().getWeight())))
  case err => println(err)
}
def createGraph(currencies: Map[String, Double]) = {
  val graph = new SimpleDirectedWeightedGraph[String, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
  val map = currencies
    .toSeq
    .map { case (fromTo, weight) => (fromTo.split("_")(0), (fromTo.split("_")(1), weight)) }
    .groupBy(_._1)

  map.keySet.foreach(graph.addVertex)
  map.foreachEntry {
    case (key, values) =>
      values.filter { case (v, e) => e._1 != v }.map(_._2).foreach {
        case (edge, weight) =>
          val e = graph.addEdge(key, edge)
          graph.setEdgeWeight(e, -scala.math.log(weight))
      }
  }
  graph
}
