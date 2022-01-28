case class ArbitragePossibilities(
  source: Option[String],
  fromSourcePossibility: Option[ArbitragePossibility],
  possibilities: Vector[ArbitragePossibility]
)
object ArbitragePossibilities {
  def make(possibilities: Vector[ArbitragePossibility], source: Option[String] = None) = {
    ArbitragePossibilities(source, possibilities.find(_.isFromSource), possibilities)
  }
}

case class ArbitragePossibility(source: String, isFromSource: Boolean, nodes: Vector[PathNode]) {
  val income = nodes.map(_.cost).reduce(_ * _)
}

case class PathNode(from: String, to: String, cost: Double)

object ArbitragePossibility {
  def make(source: String, nodes: Vector[PathNode]) = {
    val isFromSource = !nodes.isEmpty && nodes.head.from == source && nodes.last.to == source
    ArbitragePossibility(source, isFromSource, nodes)
  }
}

case class Graph(vertexes: Array[String], edges: Map[String, Map[String, Edge]])

case class Edge(rate: Double) {
  val weight = -scala.math.log(rate)
}

object Graph {
  def fromApiMap(source: Map[String, Double]) = {
    val edges = source.map {
      case (fromTo, value) =>
        val (from, to) = parseCurrenciesKey(fromTo).left.map(throw _).merge
        (from, to, value)
    }.groupBy(_._1).map {
      case (vertex, e) =>
        vertex -> e.map { case (_, to, value) => to -> Edge(value) }.toMap
    }.toMap

    Graph(edges.keySet.toArray, edges)
  }

  private def parseCurrenciesKey(fromTo: String) = {
    val array = fromTo.split("_")
    if (array.length != 2) {
      Left(new Exception("Unparsable key"))
    } else {
      Right((array(0), array(1)))
    }
  }
}
