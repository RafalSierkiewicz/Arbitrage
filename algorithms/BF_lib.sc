import $ivy.`org.jgrapht:jgrapht-core:1.5.1`, org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.alg.shortestpath.BellmanFordShortestPath
import org.jgrapht.alg.shortestpath.NegativeCycleDetectedException
import scala.jdk.CollectionConverters._
class BF(graph: SimpleDirectedWeightedGraph[String, DefaultWeightedEdge]) {
  val alg = new BellmanFordShortestPath(graph)

  def arbitrage(source: String) = {
    try {
      alg.getPaths(source)
    } catch {
      case e: NegativeCycleDetectedException =>
        println(e.getCycle().getVertexList().asScala.toList.mkString(" -> "))
        println("Income " + scala.math.exp(scala.math.abs(e.getCycle().getWeight())))

      case err: Throwable => println(err)
    }
  }
}

object Graph {
  def fromApiMap(currencies: Map[String, Double]) = {
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
}
